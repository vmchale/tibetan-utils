{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec.Lexer.Tibetan
import Text.Megaparsec
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "parseNumber" $ do
        it "parses Tibetan numerals as an Int" $ do
            (runParser parseNumber "") tibStr `shouldParse` 6320
        it "fails when given a character that isn't a tibetan numeral" $ do
            (runParser parseNumber "") `shouldFailOn` otherStr

tibStr :: T.Text
tibStr = "༦༣༢༠"

otherStr :: T.Text
otherStr = "ཨ་ནི"
