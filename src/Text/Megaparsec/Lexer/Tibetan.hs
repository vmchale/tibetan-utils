module Text.Megaparsec.Lexer.Tibetan
    ( commandLine
    ) where

import Data.Composition
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import System.Environment

commandLine :: IO ()
commandLine = ((runParser parseNumber "") . T.pack . (!!0) <$> getArgs) >>= print

parseNumber :: Parser Int
parseNumber = do
    digits <- reverse <$> many parseNumeral
    pure . (`div` 10) $ foldr ((*10) .* (+)) 0 digits

parseNumeral :: Parser Int
parseNumeral = foldr (<|>) (parseDigit '༠' 0) $ zipWith parseDigit ['༠','༡','༢','༣','༤','༥','༦','༧','༨','༩'] (0:[1..9])
--no null parser? b/c no monoid Int idk

parseDigit :: Char -> Int -> Parser Int
parseDigit c i = do
    char c
    pure i
