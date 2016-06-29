{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Hspec

import Data.Text.I18n
import Data.Text.I18n.Po

main :: IO ()
main = do
  a <- testSpec "Integration Specs" integrationSpecs
  defaultMain (tests $ testGroup "All specs" [ a ])

tests :: TestTree -> TestTree
tests specs = testGroup "i18n Tests" [ specs ]

integrationSpecs :: Spec
integrationSpecs = describe "localize" $ do
  context "when the translation does not exist" $ do
    it "should return the original" $ do
      (l10n, _) <- getL10n "./test"
      let subject = localize l10n (Locale "fr") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

  context "when the translation does exist" $ do
    it "should return the original" $ do
      (l10n, _) <- getL10n "./test"
      let subject = localize l10n (Locale "en") (gettext "Like tears in rain.")
      let result = "Like tears in rain."
      subject `shouldBe` result

    it "should return the translation" $ do
      (l10n, _) <- getL10n "./test"
      let subject = localize l10n (Locale "cy") (gettext "Like tears in rain.")
      let result = "Fel dagrau yn y glaw."
      subject `shouldBe` result
