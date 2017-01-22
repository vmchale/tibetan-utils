{-# LANGUAGE FlexibleContexts #-}

-- | Parser to parse Tibetan numerals
module Text.Megaparsec.Lexer.Tibetan
    ( parseNumber
    , readBo
    ) where

import Data.Composition
import Data.Either.Combinators
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Prim
import System.Environment

-- | Yields a command line parser in case you want a command-line executable for use with another language
--
-- > λ > readBo "༣༢༠༥"
-- > 3205
readBo :: String -> Maybe Integer
readBo = rightToMaybe . (runParser parseNumber "") . T.pack

-- | Parse Tibetan numerals, returning a positive integer
parseNumber :: Parser Integer
parseNumber = do
    digits <- reverse <$> some parseNumeral
    (pure . (`div` 10) $ foldr ((*10) .* (+)) 0 digits) <?> "tibetan integer"

-- | Parse a single digit
parseNumeral :: Parser Integer
parseNumeral = foldr (<|>) (parseDigit '༠' 0) $ zipWith parseDigit "༠༡༢༣༤༥༦༧༨༩" (0:[1..9])

-- | Parser a given char as a given integer
parseDigit :: Char -> Integer -> Parser Integer
parseDigit c i = do
    char c
    pure i
