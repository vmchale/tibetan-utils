{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Parser to parse Tibetan numerals
module Text.Megaparsec.Lexer.Tibetan
    ( parseNumber
    , readBo
    ) where

import Data.Composition
import Data.Either.Combinators
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Prim
import System.Environment

-- | Read a string in, returning integral value or error
--
-- > λ > readBo "༣༢༠༥"
-- > Just 3205
readBo :: (Integral a) => String -> Maybe a
readBo = fmap fromIntegral . rightToMaybe . (runParser (parseNumber :: Parser Integer) "")

-- | Return verbose errors.
readBoV :: (Integral a) => String -> Either (ParseError Char Dec) a
readBoV = fmap fromIntegral . (runParser (parseNumber :: Parser Integer) "")

-- | Parse Tibetan numerals, returning a positive integer
parseNumber :: (Integral a, MonadParsec e s m, Token s ~ Char) => m a
parseNumber = do
    digits <- reverse <$> some parseNumeral
    (pure . (`div` 10) $ foldr ((*10) .* (+)) 0 digits) <?> "tibetan integer"

-- | Parse a single digit
parseNumeral :: (Integral a, MonadParsec e s m, Token s ~ Char) => m a
parseNumeral = foldr (<|>) (parseDigit '༠' 0) $ zipWith parseDigit "༠༡༢༣༤༥༦༧༨༩" (0:[1..9])

-- | m a given char as a given integer
parseDigit :: (Integral a, MonadParsec e s m, Token s ~ Char) => Char -> a -> m a
parseDigit c i = do
    char c
    pure i
