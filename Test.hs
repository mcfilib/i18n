 import Text.I18n.Po
 import Prelude hiding (putStr,putStrLn)

 main = do
     (l10n,errors) <- getL10n "." -- directory containing PO files
     putStrLn $ localize l10n (Locale "en") (example "Joe")

 example :: String -> I18n String
 example name = do
     hello <- gettext "Hello, %s!"
     return (hello name)
