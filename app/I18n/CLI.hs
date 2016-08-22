module I18n.CLI where

import Data.Monoid ((<>))
import Data.Version (showVersion)
import Options.Applicative
import Paths_i18n (version)

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

-- | Description of all available subcommands.
data Command = Find FindOpts
             | ToJS ToJSOpts

-- | Runnable description of i18n CLI.
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
  command "tojs" (parseToJS `withInfo` "Convert PO files to JavaScript")

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
