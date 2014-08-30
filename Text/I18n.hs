{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.I18n
-- Copyright   :  (c) Eugene Grigoriev, 2008
-- License     :  BSD3
-- 
-- Maintainer  :  eugene.grigoriev@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Internationalization support for Haskell.
--  Use Text.I18n.Po module.
--
--  Plural forms are not yet implemented.
--
-----------------------------------------------------------------------------
module Text.I18n (
        -- * Type Declarations
        Msgid(..), Msgstr, Locale(..), Context, I18n, L10n,
        -- * Internationalization Monad Functions
        gettext, localize, withContext, withLocale
    ) where

import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Maybe
import Text.I18n.Printf
import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- Type declarations
-------------------------------------------------------------------------------
newtype Msgid = Msgid String deriving (Show,Eq,Ord)

type Msgstr = String

newtype Locale = Locale String deriving (Show,Eq,Ord)

type Context = String

-- | The Internationalization monad built using monad transformers.
type I18n a = ReaderT (Locale, L10n, Maybe Context) Identity a

-- | The Localization structure.
type L10n = Map.Map Locale
                    (Map.Map (Maybe Context)
                             (Map.Map Msgid
                                      [Msgstr]))

instance PrintfType (I18n String) where
    spr fmts args = return (uprintf fmts (reverse args))

-------------------------------------------------------------------------------
-- I18N Monad functions
-------------------------------------------------------------------------------
{-|
    The top level localization function.

    > import Text.I18n.Po
    > import Prelude hiding (putStr,putStrLn)
    >
    > main = do
    >     (l10n,errors) <- getL10n "dir/to/po" -- directory containing PO files
    >     putStrLn $ localize l10n (Locale "en") (example "Joe")
-}
localize :: L10n    -- ^ Structure containing localization data
         -> Locale  -- ^ Locale to use
         -> I18n a  -- ^ Inernationalized expression
         -> a       -- ^ Localized expression
localize l10n loc expression = runIdentity $ runReaderT expression (loc,l10n,Nothing)

{-|
    The heart of I18n monad. Based on 'Text.Printf.printf'.

    > example :: String -> I18n String
    > example name = do
    >     hello <- gettext "Hello, %s!"
    >     return (hello name)
-}
gettext :: PrintfType a => String -> I18n a
gettext msgid = do
    (loc, l10n, ctxt) <- ask
    case localizeMsgid l10n loc ctxt (Msgid msgid) of
        Just msgstr -> return (printf msgstr)
        Nothing     -> case ctxt of
                            Just _  -> withContext Nothing (gettext msgid)
                            Nothing -> return (printf msgid)

{-|
    Sets a local 'Context' for an internationalized expression.
    If there is no translation, then no context version is tried.

    > example2 :: String -> I18n String
    > example2 = withContext (Just "test") . example
-}
withContext :: Maybe Context -- ^ Context to use
            -> I18n a        -- ^ Internationalized expression
            -> I18n a        -- ^ New internationalized expression
withContext ctxt expression = do
    (lang, l10n, _) <- ask
    local (const (lang, l10n, ctxt))
          expression

{-|
    Sets a local 'Locale' for an internationalized expression.

    > example3 :: String -> I18n String
    > example3 = withLocale (Locale "ru") . example2
-}
withLocale :: Locale    -- ^ Locale to use
           -> I18n a    -- ^ Internationalized expression
           -> I18n a    -- ^ New internationalized expression.
    -- Note: while this expression is localy localized already, it is to be a
    -- part of another internationalized expression.
    -- Therefore the final type is internationalized.
withLocale loc expression = do
    (_, l10n, ctxt) <- ask
    local (const (loc, l10n, ctxt))
          expression

localizeMsgid :: L10n -> Locale -> Maybe Context -> Msgid -> Maybe String
localizeMsgid l10n loc ctxt msgid = do
    local      <- Map.lookup loc  l10n
    contextual <- Map.lookup ctxt local
    msgstrs    <- Map.lookup msgid contextual
    listToMaybe msgstrs
