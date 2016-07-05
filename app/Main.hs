{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text.I18n.Shakespeare
import           Data.Version
import           Options.Applicative
import           Paths_i18n
import           System.Exit
import           System.IO (withFile, IOMode(..), hPutStr)
import           Text.RawString.QQ

-- | Description of shakespeare-gettext CLI.
data CLIOpts = CLIOpts { cliKeyword :: Maybe String, cliOutput :: String, cliFiles :: [String] }

-- | Runnable description of shakespeare-gettext.
cliOpts :: ParserInfo CLIOpts
cliOpts =
  info (helper <*> cliVersion <*> cliOptsParser)
    (fullDesc
     <> progDesc "xgettext clone for the Haskell ecosystem"
     <> header "i18n - xgettext for Haskell")

-- | Show version number from the cabal file.
cliVersion :: Parser (a -> a)
cliVersion = infoOption (showVersion version)
               (long "version" <> short 'v' <> help "Show version information" <> hidden)

-- | Parser for shakespeare-gettext.
cliOptsParser :: Parser CLIOpts
cliOptsParser =
  CLIOpts <$> optional
                (strOption
                   (long "keyword"
                    <> short 'k'
                    <> help "Name of gettext function"))
                <*> strOption
                   (long "output"
                    <> short 'o'
                    <> help "Write output to specified file")
                <*> some (argument str (metavar "FILES..."))

-- | Interpret description of shakespeare-gettext in IO.
runCli :: CLIOpts -> IO ()
runCli CLIOpts { .. } = do
  writeFile cliOutput potHeader

  withFile cliOutput AppendMode $ \hld -> do
    translations <- foldM translationFolder Set.empty cliFiles
    forM_ translations (hPutStr hld)
  where
    translationFolder :: Set.Set String -> FilePath -> IO (Set String)
    translationFolder !acc path = do
        source <- readFile path
        let result = decode (fromMaybe defaultKeyword cliKeyword) source
        case result of
          Left err ->
            die err
          Right t -> return $ acc <> toMessages t

    defaultKeyword :: String
    defaultKeyword = "_"


-- | Transform translations to collection of messages.
toMessages :: Set.Set String -> Set.Set String
toMessages = Set.map messageTemplate
    where
      messageTemplate :: String -> String
      messageTemplate translation = unlines [ "msgid " <> show translation
                                            , "msgstr \"\""
                                            , ""
                                            ]

-- | POT header file.
potHeader :: String
potHeader = [r|# Translation file.
#
#, fuzzy
msgid ""
msgstr ""
"Project-Id-Version: PACKAGE VERSION\n"
"Report-Msgid-Bugs-To: \n"
"POT-Creation-Date: 2001-01-01 06:00+0100\n"
"PO-Revision-Date: 2001-01-01 06:00+0100\n"
"Last-Translator: FULL NAME <EMAIL@ADDRESS>\n"
"Language-Team: LANGUAGE <LL@li.org>\n"
"Language: \n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"
"Content-Transfer-Encoding: 8bit\n"

|]

main :: IO ()
main = execParser cliOpts >>= runCli
