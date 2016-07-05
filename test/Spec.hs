{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.I18n
import           Data.Text.I18n.Po
import           Data.Text.I18n.Shakespeare
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
  a <- testSpec "Library Specs" librarySpecs
  b <- testSpec "Application Specs" applicationSpecs
  defaultMain (tests $ testGroup "All specs" [a, b])

tests :: TestTree -> TestTree
tests specs = testGroup "i18n Tests" [specs]

librarySpecs :: Spec
librarySpecs = describe "localize" $ do
  let localeDir = "test/locale"

  context "when the translation does not exist" $
    it "should return the original" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "fr") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

  context "when the translation does exist" $ do
    it "should return the original" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "en") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

    it "should return the translation" $ do
      (l10n, _) <- getL10n localeDir
      let subject = localize l10n (Locale "cy") (gettext "Like tears in rain.")
      let result = "Fel dagrau yn y glaw."
      subject `shouldBe` result

applicationSpecs :: Spec
applicationSpecs = describe "Shakespeare parser" $
  describe "decode" $ do
    context "when the file contains some translations" $ do
      it "should be able to find them" $ do
        template <- readFile "test/templates/template.hamlet"
        let subject = decode "_" template
        let result = Right ["Hello there, %s.", "Hello there, %s.", "hello from me"]
        subject `shouldBe` result
      it "should leave decision of what to do with duplicates up to consumers" $ do
        template <- readFile "test/templates/template_with_dupes.hamlet"
        let subject = decode "_" template
        let result = Right ["Getting Started", "Getting Started"]
        subject `shouldBe` result

    context "when the file does not contain some translations" $
      it "should not find them" $ do
        template <- readFile "test/templates/template.hamlet"
        let subject = decode "gettext" template
        let result = Right mempty
        subject `shouldBe` result
