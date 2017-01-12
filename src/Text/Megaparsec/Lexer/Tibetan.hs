module Text.Megaparsec.Lexer.Tibetan
    ( commandLine
    , parseNumber
    ) where

import Data.Composition
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import System.Environment

commandLine :: IO ()
commandLine = ((runParser parseNumber "") . T.pack . (!!0) <$> getArgs) >>= print

parseNumber :: Parser Integer
parseNumber = do
    digits <- reverse <$> some parseNumeral
    pure . (`div` 10) $ foldr ((*10) .* (+)) 0 digits

parseNumeral :: Parser Integer
parseNumeral = foldr (<|>) (parseDigit '༠' 0) $ zipWith parseDigit "༠༡༢༣༤༥༦༧༨༩" (0:[1..9])

parseDigit :: Char -> Integer -> Parser Integer
parseDigit c i = do
    char c
    pure i
