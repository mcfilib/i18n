-- |
-- Module:      Data.Text.I18n.Po
-- Copyright:   (c) 2011-2016 Eugene Grigoriev
-- License:     BSD3
-- Maintainer:  Philip Cunningham <hello@filib.io>
-- Stability:   experimental
-- Portability: portable
--
-- Internationalisation support for Haskell.

module Data.Text.I18n.Types (
    -- * Type Declarations
    Context(..),
    CtxMap,
    I18n,
    L10n,
    Locale(..),
    MsgDec(..),
    Msgid(..),
    Msgstr(..),
    ) where

import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T

-- $setup
-- >>> :set -XOverloadedStrings
--
-- | Local context.
newtype Context = Context T.Text
  deriving (Eq, Ord)

-- | Mapping from a contexts to translation mappings.
type CtxMap = Map.Map (Maybe Context) TranslationMap

-- | The Internationalization monad built using monad transformers.
type I18n a = ReaderT (Locale, L10n, Maybe Context) Identity a

-- | The Localization structure.
type L10n = Map.Map Locale (Map.Map (Maybe Context) (Map.Map Msgid [Msgstr]))

-- | Textual representation of a locale e.g.
-- >>> Locale "en_GB"
-- ...
newtype Locale = Locale T.Text
  deriving (Show, Eq, Ord)

-- | Message delcaration.
data MsgDec = MsgDec (Maybe Context) Msgid [Msgstr]

-- | Untranslated string.
-- >>> Msgid "hello there"
-- ...
newtype Msgid = Msgid T.Text
  deriving (Show, Eq, Ord)

-- | Translated string.
newtype Msgstr = Msgstr T.Text

-- | Mapping from identifiers to translations.
type TranslationMap = Map.Map Msgid [Msgstr]
