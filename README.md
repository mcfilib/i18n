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
