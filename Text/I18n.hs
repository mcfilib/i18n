{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
    Msgid(..)
  , Context
  , I18n
  , L10n
  , Locale(..)
  , Msgstr
    -- * Internationalization Monad Functions
  , gettext
  , localize
  , withContext
  , withLocale
) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Type declarations
-------------------------------------------------------------------------------
newtype Msgid = Msgid T.Text deriving (Show, Eq, Ord)

type Msgstr = T.Text

newtype Locale = Locale T.Text deriving (Show, Eq, Ord)

type Context = T.Text

-- | The Internationalization monad built using monad transformers.
type I18n a = ReaderT (Locale, L10n, Maybe Context) Identity a

-- | The Localization structure.
type L10n = Map.Map Locale
                    (Map.Map (Maybe Context)
                             (Map.Map Msgid
                                      [Msgstr]))

-------------------------------------------------------------------------------
-- I18N Monad functions
-------------------------------------------------------------------------------
{-|
    The top level localization function.

    > {-# LANGUAGE OverloadedStrings #-}
    >
    > import qualified Data.Text.IO as TIO
    > import           Text.I18n.Po
    >
    > main = do
    >     (l10n,errors) <- getL10n "dir/to/po" -- directory containing PO files
    >     TIO.putStrLn $ localize l10n (Locale "en") (example "Joe")
-}
localize :: L10n    -- ^ Structure containing localization data
         -> Locale  -- ^ Locale to use
         -> I18n a  -- ^ Inernationalized expression
         -> a       -- ^ Localized expression
localize l10n loc expression = runIdentity $ runReaderT expression (loc, l10n, Nothing)

{-|
    The heart of I18n monad. Based on 'Text.Printf.printf'.

    > example :: T.Text -> I18n T.Text
    > example name = do
    >     hello <- gettext "Hello, %s!"
    >     return (hello name)
-}
gettext :: T.Text -> I18n T.Text
gettext msgid = do
    (loc, l10n, ctxt) <- ask
    case localizeMsgid l10n loc ctxt (Msgid msgid) of
        Just msgstr -> return msgstr
        Nothing     -> case ctxt of
                            Just _  -> withContext Nothing (gettext msgid)
                            Nothing -> return msgid

{-|
    Sets a local 'Context' for an internationalized expression.
    If there is no translation, then no context version is tried.

    > example2 :: T.Text -> I18n T.Text
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

    > example3 :: T.Text -> I18n T.Text
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

localizeMsgid :: L10n -> Locale -> Maybe Context -> Msgid -> Maybe T.Text
localizeMsgid l10n loc ctxt msgid = do
    local'     <- Map.lookup loc l10n
    contextual <- Map.lookup ctxt local'
    msgstrs    <- Map.lookup msgid contextual
    listToMaybe msgstrs
