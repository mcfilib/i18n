-----------------------------------------------------------------------------
-- |
-- Module      :  Text.I18n.Po
-- Copyright   :  (c) Eugene Grigoriev, 2008
-- License     :  BSD3
-- 
-- Maintainer  :  eugene.grigoriev@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Internationalization support for Haskell. This module contains
--  PO parser. PO files are assumed to be in UTF-8 encoding.
--
--  Plural forms are not yet implemented.
--
--  modules Text.I18n, Control.Monad.Trans, and function putStrLn and putStr
--  from System.IO.UTF8 are exported for convenience.
--
-- > import Prelude hiding (putStr,putStrLn)
--
-----------------------------------------------------------------------------
module Text.I18n.Po (
        -- * Type Declarations
        Msgid(..), Msgstr, Locale(..), Context, I18n, L10n,
        -- * PO parsing
        getL10n,
        -- * I18n Monad Functions
        localize, gettext, withContext, withLocale,
        -- * UTF-8 Output Functions
        Utf8.putStrLn, Utf8.putStr,
        module Control.Monad.Trans
    ) where

import Text.I18n
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Trans
import Control.Arrow
import System.Directory
import System.FilePath
import qualified System.IO.UTF8 as Utf8

-------------------------------------------------------------------------------
-- Type declarations
-------------------------------------------------------------------------------
data MsgDec = MsgDec (Maybe Context) Msgid [Msgstr]

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------
-- | Builds 'L10n' structure by parsing / .po / files contained in a given
-- directory. 'L10n' structure is to be passed to 'localize' function.
-- 'L10n' structure is used internaly by the 'I18n' monad.
getL10n :: FilePath                 -- ^ Directory containing PO files.
        -> IO (L10n, [ParseError])  -- ^ Localization structure
            -- and a list of parse errors.
getL10n dir = do
    poFiles   <- poFiles dir
    locs      <- processPos (map (second parsePo) poFiles)
    (es,locs) <- return (separateEithers locs)
    return (Map.fromList locs, es)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------
processPos :: [(Locale, IO (Either ParseError [MsgDec]))]
           -> IO [Either ParseError
                         (Locale, Map.Map (Maybe Context)
                                          (Map.Map Msgid
                                                   [Msgstr]))]
processPos rs = do
    rs <- mapM (\(a,m) -> m >>= \b -> return (a,b)) rs
    return $ map f rs
  where f (l, (Right msgdecs)) = Right (l, mkMsgs msgdecs)
        f (_, (Left  e))       = Left e

mkMsgs :: [MsgDec] -> Map.Map (Maybe Context) (Map.Map Msgid [Msgstr])
mkMsgs = foldl' f mempty
    where f m (MsgDec ctxt msgid msgstrs) = case Map.lookup ctxt m of
            Nothing -> f (Map.insert ctxt mempty m) (MsgDec ctxt msgid msgstrs)
            Just c  -> Map.insert ctxt (Map.insert msgid msgstrs c) m

poFiles :: FilePath -> IO [(Locale,FilePath)]
poFiles dir = do
    files <- getDirectoryContents dir
    return $ (map ((&&&) (Locale . (subtract 3 . length >>= take))
                         (intercalate [pathSeparator] . (dir :) . return))
              . filter (flip any [".po",".Po",".PO"] . flip isSuffixOf))
             files

parsePo :: FilePath -> IO (Either ParseError [MsgDec])
parsePo n = do
    contents <- Utf8.readFile n
    return $ parse po n contents

-------------------------------------------------------------------------------
-- .po Parser
-------------------------------------------------------------------------------
{- EBNF
    PO            ::= msg*
    msg           ::= [msg-context] (msg-singular | msg-plural)
    msg-context   ::= "msgctxt" string*
    msg-singular  ::= msgid msgstr
    msg-plural    ::= msgid msgid-plural msgstr-plural*
    msgid         ::= "msgid"  string*
    msgid-plural  ::= "msgid_plural" string*
    msgstr        ::= "msgstr" string*
    msgstr-plural ::= "msgstr" form string*
    form          ::= "[" number "]"
    number        ::= (0-9)* | "N"
    string        ::= "\"" (char | escaped-char)* "\""
    escaped-char  ::= "\\" char
    char          ::= (any UTF8 character)
-}
lexer = P.makeTokenParser (emptyDef {
    commentLine   = "#",
    reservedNames = ["msgctxt","msgid","msgid_plural","msgstr"] })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme     lexer
reserved   = P.reserved   lexer

po = do whiteSpace
        msgs <- many msg 
        eof
        return msgs

msg = do ctxt      <- msg_context
         (id,strs) <- try msg_singular <|> msg_plural
         return (MsgDec ctxt id strs)

msg_context = try $ option Nothing $ do
    lexeme (reserved "msgctxt")
    strs <- many1 str
    return (Just (concat strs))

msg_singular = do id  <- lexeme msgid
                  str <- lexeme msgstr
                  return (Msgid id, [str])

msg_plural = do id    <- lexeme msgid
                idp   <- lexeme msgid_plural
                strps <- lexeme (many1 msgstr_plural)
                return (Msgid id, strps)

msgid = do lexeme (reserved "msgid")
           strs <- many1 str
           return (concat strs)

msgid_plural = do lexeme (reserved "msgid_plural")
                  strs <- many1 str
                  return (concat strs)

msgstr = do lexeme (reserved "msgstr")
            strs <- many1 str
            return (concat strs)

msgstr_plural = do lexeme (reserved "msgstr")
                   char '['
                   try (do c <- oneOf ['n','N']
                           return [c])
                       <|> many1 (oneOf ['0'..'9'])
                   char ']'
                   whiteSpace
                   strs <- many1 str
                   return (concat strs)

str = lexeme $ do char '"'
                  chs <- many ch
                  char '"'
                  return chs

ch = try escaped_ch <|> noneOf ['"']

escaped_ch = do e <- char '\\'
                c <- anyChar
                case reads ['\'',e,c,'\''] :: [(Char,String)] of
                    (c,s):[] -> return c
                    _        -> return c

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
separateEithers = foldr (either (first . (:)) (second . (:))) ([],[])
