# i18n

A suite of tools for dealing with internationalisation in Haskell applications.

## Usage

### Library

``` haskell
-- Formatted for use in ghci.

:set -XOverloadedStrings

:{
import qualified Data.Text.I18n    as I18n
import qualified Data.Text.I18n.Po as I18n

-- Setup i18n context.
(l10n, _) <- I18n.getL10n "./test/locale"

-- Add an example annotation.
let example = I18n.gettext "Like tears in rain."

-- Localise the example.
I18n.localize l10n (I18n.Locale "cym") example
:}
```

### Command-line Application

The `i18n` command line application is composed of several, useful, subcommands.

```
i18n - xgettext for Haskell

Usage: i18n COMMAND
  xgettext clone for the Haskell ecosystem

Available options:
  -h,--help                Show this help text
  -v,--version             Show version information

Available commands:
  find                     Find translations in src files
  tojs                     Convert PO files to JS
```

It enables you to find `gettext` annotations in your code.

```
Usage: i18n find [-k|--keyword ARG] (-o|--output ARG) [-r|--regex ARG] FILES...
  Find translations in src files

Available options:
  -h,--help                Show this help text
  -k,--keyword ARG         Name of gettext function
  -o,--output ARG          Write output to specified file
  -r,--regex ARG           Regexp for extracting annotations
```

For example, you can pull annotated strings from `hamlet` templates e.g.

```
$ i18n find --keyword _ --output i18n.pot test/templates/template.hamlet
```

It enables you to convert `.po` files to different formats.

```
Usage: i18n tojs (-p|--po ARG) (-o|--output ARG) (-l|--locale ARG)
  Convert PO files to JS

Available options:
  -h,--help                Show this help text
  -p,--po ARG              PO file to parse
  -o,--output ARG          Write output to specified file
  -l,--locale ARG          Locale e.g. en_GB
```

For example, you can convert `.po` files to `JavaScript` e.g.

```
$ i18n tojs --po test/locale/en.po --output test/locale/en.js --locale en
```

## Development

### Tasks

```
$ make help
clean                          Clean Haskell local packages
format                         Format Haskell source
help                           Print available tasks
install                        Compile Haskell binary
repl                           Launch ghci
spec                           Run the specs
watch                          Compile on file changes
```

### Tests

```
$ make spec
Examples: 11  Tried: 11  Errors: 0  Failures: 0
Examples: 3  Tried: 3  Errors: 0  Failures: 0
shakespeare-2.0.8.2: test (suite: test)
i18n-0.4.0.0: test (suite: i18n-test)
Completed 2 action(s).
```
