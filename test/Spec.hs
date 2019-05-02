-- vim: syntax=hspec
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                      as T
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Text.Megaparsec.Char.Tibetan
import           Text.Megaparsec.Lexer.Tibetan
import           TextShow.Data.Integral.Tibetan

main :: IO ()
main = hspec $ do
    describe "parseNumber" $ do
        it "parses Tibetan numerals as an Int" $
            runParser (parseNumber :: Parser Integer) "" tibStr `shouldParse` 6320
        it "fails when given a character that isn't a tibetan numeral" $
            runParser (parseNumber :: Parser Integer) "" `shouldFailOn` otherStr
    describe "showBo" $ do
        it "displays a positive integer using tibetan numerals" $
            showBo 15 `shouldBe` Just "༡༥"
        it "fails on negative inputs" $
            showBo (-12) `shouldBe` Nothing

tibStr :: String
tibStr = "༦༣༢༠"

otherStr :: String
otherStr = "ཨ་ནི"
