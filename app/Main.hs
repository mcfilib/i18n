{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.List                  as List
import           Data.Maybe
import qualified Data.Set                   as Set
import           Data.String.Conv
import           Data.Text.I18n.Shakespeare
import           Data.Version
import           Options.Applicative
import           Paths_i18n
import           System.Exit
import           System.IO                  (IOMode (..), hPutStr, withFile)
import           Text.RawString.QQ
import qualified Text.Regex.PCRE.Light      as PCRE

-- | Description of shakespeare-gettext CLI.
data CLIOpts =
       CLIOpts
         { cliKeyword :: Maybe String
         , cliOutput  :: String
         , cliRegexp  :: Maybe String
         , cliFiles   :: [String]
         }

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
  CLIOpts <$> parseKeyword <*> parseOutput <*> parseRegexp <*> parseFiles
  where
    parseKeyword :: Parser (Maybe String)
    parseKeyword = optional
                     (strOption
                        (long "keyword"
                         <> short 'k'
                         <> help "Name of gettext function"))

    parseOutput :: Parser String
    parseOutput = strOption
                    (long "output"
                     <> short 'o'
                     <> help "Write output to specified file")

    parseFiles :: Parser [String]
    parseFiles = some (argument str (metavar "FILES..."))

    parseRegexp :: Parser (Maybe String)
    parseRegexp = optional
                    (strOption
                       (long "regex"
                        <> short 'r'
                        <> help "Regexp for extracting annotations"))

-- | Interpret description of shakespeare-gettext in IO.
runCli :: CLIOpts -> IO ()
runCli CLIOpts { .. } = do
  writeFile cliOutput potHeader

  withFile cliOutput AppendMode $ \fileHandle -> do
    translations <- Set.fromList <$> foldM gatherTranslations mempty cliFiles
    forM_ translations (hPutStr fileHandle)

  where
    gatherTranslations :: [String] -> FilePath -> IO [String]
    gatherTranslations !acc path = do
      source <- readFile path
      case cliRegexp of
        Nothing     -> handleOthers acc source
        Just regexp -> handleRegexp acc source (PCRE.compile (toS regexp) [])

    handleRegexp :: [String] -> String -> PCRE.Regex -> IO [String]
    handleRegexp !acc source regexp = foldM go mempty (lines source)
      where
        go :: [String] -> String -> IO [String]
        go !acc' line =
          case PCRE.match regexp (toS line) [] of
            Nothing -> return acc'
            Just matches ->
              case matches of
                []        -> return acc
                [_]       -> return acc
                _:match:_ -> return $! acc <> List.insert (toMessage (toS match)) acc'

    handleOthers :: [String] -> String -> IO [String]
    handleOthers !acc source = do
      let result = decode (fromMaybe defaultKeyword cliKeyword) source
      case result of
        Left err ->
          die err
        Right translations ->
          return $! acc <> fmap toMessage translations

    defaultKeyword :: String
    defaultKeyword = "_"

-- | Transform a translation to a message.
toMessage :: String -> String
toMessage translation = unlines ["msgid " <> show translation, "msgstr \"\"", ""]

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
