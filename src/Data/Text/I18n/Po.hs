{-# LANGUAGE OverloadedStrings #-}

-- --------------------------------------------------------------------------- | Module :
-- Data.Text.I18n.Po Copyright : (c) Eugene Grigoriev, 2008 License : BSD3
--
-- Maintainer : eugene.grigoriev@gmail.com Stability : experimental Portability : portable
--
-- Internationalization support for Haskell. This module contains
--  PO parser. PO files are assumed to be in UTF-8 encoding.
--
--  Plural forms are not yet implemented.
--
-----------------------------------------------------------------------------
module Data.Text.I18n.Po (
    -- * PO parsing
    getL10n,
    -- * I18n Monad Functions
    localize,
    gettext,
    withContext,
    withLocale,
    ) where

import           Control.Arrow                          (second, (&&&))
import           Data.Either                            (partitionEithers)
import           Data.Functor.Identity                  (Identity)
import           Data.List                              (foldl', intercalate,
                                                         isSuffixOf)
import qualified Data.Map                               as Map
import qualified Data.Text                              as T
import           Data.Text.I18n
import           Data.Text.I18n.Types
import qualified Data.Text.IO                           as T
import           System.Directory                       (getDirectoryContents)
import           System.FilePath                        (pathSeparator)
import           Text.Parsec
import           Text.Parsec.Text
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

-- ----------------------------------------------------------------------------- Interface
-- ----------------------------------------------------------------------------- | Builds 'L10n'
-- structure by parsing / .po / files contained in a given directory. 'L10n' structure is to be
-- passed to 'localize' function. 'L10n' structure is used internaly by the 'I18n' monad.
getL10n :: FilePath
        ->
           -- ^ Directory containing PO files.
           IO (L10n, [ParseError])
-- ^ Localization structure and a list of parse errors.
getL10n dir = do
  poFiles' <- poFiles dir
  locs <- processPos (map (second parsePo) poFiles')
  (es, locs') <- return $! partitionEithers locs
  return (Map.fromList locs', es)

-- ----------------------------------------------------------------------------- Internal
-- -----------------------------------------------------------------------------
processPos :: [(Locale, IO (Either ParseError [MsgDec]))]
           -> IO [Either ParseError (Locale, CtxMap)]
processPos rs = do
  rs' <- mapM (\(a, m) -> m >>= \b -> return (a, b)) rs
  return $! map f rs'

  where
    f :: (a, Either b [MsgDec]) -> Either b (a, CtxMap)
    f (l, Right msgdecs) = Right (l, mkMsgs msgdecs)
    f (_, Left e) = Left e

mkMsgs :: [MsgDec] -> CtxMap
mkMsgs = foldl' f mempty
  where
    f :: CtxMap -> MsgDec -> CtxMap
    f m (MsgDec ctxt msgid' msgstrs) =
      case Map.lookup ctxt m of
        Nothing -> f (Map.insert ctxt mempty m) (MsgDec ctxt msgid' msgstrs)
        Just c  -> Map.insert ctxt (Map.insert msgid' msgstrs c) m

-- | Finds all .po files for a given directory. Works on the assumption that the characters before
-- the file extension are the locale name.
poFiles :: FilePath -> IO [(Locale, FilePath)]
poFiles dir = do
  files <- getDirectoryContents dir
  return $! fmap assocLocalesAndPaths <$> onlyPoFiles $ files

  where
    toAbsolutePath :: FilePath -> FilePath
    toAbsolutePath = intercalate [pathSeparator] . (dir :) . return

    isPoFile :: FilePath -> Bool
    isPoFile = flip any [".po", ".Po", ".PO"] . flip isSuffixOf

    onlyPoFiles :: [FilePath] -> [FilePath]
    onlyPoFiles = filter isPoFile

    assocLocalesAndPaths :: FilePath -> (Locale, FilePath)
    assocLocalesAndPaths = stripLocale &&& toAbsolutePath

    stripLocale :: FilePath -> Locale
    stripLocale path =
      let n = subtract 3 . length $ path
      in Locale . T.pack $! take n path

parsePo :: FilePath -> IO (Either ParseError [MsgDec])
parsePo path = do
  contents <- T.readFile path
  return $! parse po path contents

-- ----------------------------------------------------------------------------- .po Parser
-- -----------------------------------------------------------------------------
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
lexer :: P.GenTokenParser T.Text () Identity
lexer = P.makeTokenParser poLangDef
  where
    poLangDef :: GenLanguageDef T.Text st Identity
    poLangDef = LanguageDef
      { commentStart = ""
      , commentEnd = ""
      , commentLine = "#"
      , nestedComments = True
      , identStart = letter <|> char '_'
      , identLetter = alphaNum <|> oneOf "_'"
      , opStart = opLetter poLangDef
      , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , reservedOpNames = []
      , reservedNames = ["msgctxt", "msgid", "msgid_plural", "msgstr"]
      , caseSensitive = True
      }

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

po :: Parser [MsgDec]
po = do
  _ <- whiteSpace
  msgs <- many msg
  _ <- eof
  return $! msgs

msg :: Parser MsgDec
msg = do
  ctxt <- msgContext
  (id', strs) <- try msgSingular <|> msgPlural
  return $! MsgDec ctxt id' strs

msgContext :: Parser (Maybe Context)
msgContext = try $ option Nothing $ do
  _ <- lexeme (reserved "msgctxt")
  strs <- many1 str
  return (Just $! mconcat strs)

msgSingular :: Parser (Msgid, [Msgstr])
msgSingular = do
  id' <- lexeme msgid
  str' <- lexeme msgstr
  return (Msgid id', [str'])

msgPlural :: Parser (Msgid, [Msgstr])
msgPlural = do
  id' <- lexeme msgid
  _ <- lexeme msgidPlural
  strps <- lexeme (many1 msgstrPlural)
  return (Msgid id', strps)

msgid :: Parser T.Text
msgid = do
  _ <- lexeme (reserved "msgid")
  strs <- many1 str
  return $! mconcat strs

msgidPlural :: Parser Msgstr
msgidPlural = do
  _ <- lexeme (reserved "msgid_plural")
  strs <- many1 str
  return $! mconcat strs

msgstr :: Parser Msgstr
msgstr = do
  _ <- lexeme (reserved "msgstr")
  strs <- many1 str
  return $! mconcat strs

msgstrPlural :: Parser Msgstr
msgstrPlural = do
  _ <- lexeme (reserved "msgstr")
  _ <- char '['
  _ <- try indice
  _ <- char ']'
  _ <- whiteSpace
  strs <- many1 str
  return $! mconcat strs

  where
    caseN :: Parser String
    caseN = do
      c <- oneOf ['n', 'N']
      return [c]

    caseX :: Parser String
    caseX = many1 $ oneOf ['0' .. '9']

    indice :: Parser String
    indice = caseN <|> caseX

str :: Parser T.Text
str = lexeme $ do
  _ <- char '"'
  chs <- many char'
  _ <- char '"'
  return $! T.pack chs

char' :: Parser Char
char' = try escapedChar <|> noneOf ['"']

escapedChar :: Parser Char
escapedChar = do
  e <- char '\\'
  c <- anyChar
  case reads ['\'', e, c, '\''] :: [(Char, String)] of
    [(c', _)] -> return $! c'
    _         -> return $! c
