{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.List as List
import           Data.Maybe
import qualified Data.Set as Set
import           Data.String.Conv
import qualified Data.Text.I18n.Po as I18n
import           Data.Text.I18n.Shakespeare
import           Data.Text.I18n.Types
import           Data.Version
import           Language.Javascript.JMacro
import           Options.Applicative
import           Paths_i18n
import           System.Exit
import           System.IO (IOMode (..), hPutStr, withFile)
import           Text.RawString.QQ
import qualified Text.Regex.PCRE.Light as PCRE

-- | Description of all available subcommands.
data Command = Find FindOpts | ToJS ToJSOpts

-- | Description of i18n tojs CLI.
data ToJSOpts = ToJSOpts { toJSPOFile :: FilePath
                         , toJSOutput :: FilePath
                         , toJSLocale :: String
                         }

-- | Description of i18n find CLI.
data FindOpts =
       FindOpts
         { findKeyword :: Maybe String
         , findOutput  :: FilePath
         , findRegexp  :: Maybe String
         , findFiles   :: [FilePath]
         }

-- | Runnable description of shakespeare-gettext.
cliInfo :: ParserInfo Command
cliInfo =
  info (helper <*> cliVersion <*> parseCommand)
    (fullDesc
     <> progDesc "xgettext clone for the Haskell ecosystem"
     <> header "i18n - xgettext for Haskell")

-- | Show version number from the cabal file.
cliVersion :: Parser (a -> a)
cliVersion = infoOption (showVersion version)
               (long "version" <> short 'v' <> help "Show version information" <> hidden)

-- | Utility function to add help support.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Parser for subcommands.
parseCommand :: Parser Command
parseCommand = subparser $
  command "find" (parseFind `withInfo` "Find translations in src files") <>
  command "tojs" (parseToJS `withInfo` "Convert PO files to JS")

-- | Parser for the find subcommand.
parseFind :: Parser Command
parseFind = Find <$> parseFindOpts
  where
    parseFindOpts :: Parser FindOpts
    parseFindOpts = FindOpts <$> parseKeyword <*> parseOutput <*> parseRegexp <*> parseFiles

    parseKeyword :: Parser (Maybe String)
    parseKeyword = optional
                     (strOption
                        (long "keyword"
                         <> short 'k'
                         <> help "Name of gettext function"))

    parseOutput :: Parser FilePath
    parseOutput = strOption
                    (long "output"
                     <> short 'o'
                     <> help "Write output to specified file")

    parseFiles :: Parser [FilePath]
    parseFiles = some (argument str (metavar "FILES..."))

    parseRegexp :: Parser (Maybe String)
    parseRegexp = optional
                    (strOption
                       (long "regex"
                        <> short 'r'
                        <> help "Regexp for extracting annotations"))

-- | Parser for the tojs subcommand.
parseToJS :: Parser Command
parseToJS = ToJS <$> parseJSOpts
  where
    parseJSOpts :: Parser ToJSOpts
    parseJSOpts = ToJSOpts <$> parsePOFile <*> parseOutput <*> parseLocale

    parsePOFile :: Parser FilePath
    parsePOFile = strOption
                    (long "po"
                     <> short 'p'
                     <> help "PO file to parse")

    parseOutput :: Parser FilePath
    parseOutput = strOption
                    (long "output"
                     <> short 'o'
                     <> help "Write output to specified file")

    parseLocale :: Parser String
    parseLocale = strOption
                    (long "locale"
                     <> short 'l'
                     <> help "Locale e.g. en_GB")

-- | Interpret description of i18n in IO.
runCli :: Command -> IO ()
runCli (Find opts) = runFind opts
runCli (ToJS opts) = runToJS opts

-- | Interpret the find command in IO.
runFind :: FindOpts -> IO ()
runFind FindOpts { .. } = do
  writeFile findOutput potHeader

  withFile findOutput AppendMode $ \fileHandle -> do
    translations <- Set.fromList <$> foldM gatherTranslations mempty findFiles
    forM_ translations (hPutStr fileHandle)

  where
    gatherTranslations :: [String] -> FilePath -> IO [String]
    gatherTranslations !acc path = do
      source <- readFile path
      case findRegexp of
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
      let result = decode (fromMaybe defaultKeyword findKeyword) source
      case result of
        Left err ->
          die err
        Right translations ->
          return $! acc <> fmap toMessage translations

    defaultKeyword :: String
    defaultKeyword = "_"

-- | Interpret the tojs command in IO.
runToJS :: ToJSOpts -> IO ()
runToJS ToJSOpts { .. } = do
  result <- I18n.parsePo toJSPOFile
  case result of
    Left err  -> die (show err)
    Right messages -> do
      writeFile toJSOutput "// Autogenerated by i18n"
      withFile toJSOutput AppendMode $ \fileHandle -> do
        hPutStr fileHandle "\n"
        hPutStr fileHandle (toJS messages)
  where
    toJS :: [MsgDec] -> String
    toJS messages = show $ renderJs (defineObject <> mconcat (fmap addToObject messages))

    defineObject :: JStat
    defineObject = [jmacro| var !locale = locale || {}; |]

    addToObject :: MsgDec -> JStat
    addToObject (MsgDec _ (Msgid key) vals) = case key of
      "" -> mempty
      _  -> [jmacro| locale[`(key)`] = `(vals)`; |]

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
main = execParser cliInfo >>= runCli
