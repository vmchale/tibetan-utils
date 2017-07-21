{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec.Lexer.Tibetan
import Text.Megaparsec
import Text.Megaparsec.Text
import TextShow.Data.Integral.Tibetan
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "parseNumber" $ do
        it "parses Tibetan numerals as an Int" $
            (runParser (parseNumber :: Parser Integer) "") tibStr `shouldParse` 6320
        it "fails when given a character that isn't a tibetan numeral" $
            (runParser (parseNumber :: Parser Integer) "") `shouldFailOn` otherStr
    describe "showBo" $ do
        it "displays a positive integer using tibetan numerals" $
            showBo 15 `shouldBe` Just ("༡༥")
        it "fails on negative inputs" $
            showBo (-12) `shouldBe` Nothing

tibStr :: T.Text
tibStr = "༦༣༢༠"

otherStr :: T.Text
otherStr = "ཨ་ནི"
