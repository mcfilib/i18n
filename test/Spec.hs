{-# LANGUAGE OverloadedStrings #-}

import Data.Text.I18n
import Data.Text.I18n.Po
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  a <- testSpec "Library Specs" librarySpecs
  defaultMain (tests $ testGroup "All specs" [a])

tests :: TestTree -> TestTree
tests specs = testGroup "i18n Tests" [specs]

librarySpecs :: Spec
librarySpecs = describe "localize" $ do
  let localeDir = "test/locale"

  context "when the translation does not exist" $ do
    it "should return the original" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "fr") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

    it "should return the original" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "en") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

  context "when the translation does exist" $ do
    it "should return the translation" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "cym") (gettext "Like tears in rain.")
      let result = "Fel dagrau yn y glaw."
      subject `shouldBe` result

    context "but it is an empty string" $ do
      it "should return the original" $ do
        (l10n, _) <- getL10n localeDir
        let subject = localize l10n (Locale "fr") (gettext "Like tears in rain.")
        let result = "Like tears in rain."
        subject `shouldBe` result
