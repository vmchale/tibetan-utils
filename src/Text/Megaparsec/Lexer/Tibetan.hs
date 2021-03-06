{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Parser to parse Tibetan numerals
module Text.Megaparsec.Lexer.Tibetan
    ( parseNumber
    , readBo
    , readBoV
    ) where

import           Control.Composition
import qualified Data.Text                    as T
import           Data.Void
import           System.Environment
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Tibetan

rightToMaybe :: Either a b -> Maybe b
rightToMaybe Left{}    = Nothing
rightToMaybe (Right x) = Just x

-- | Read a string in, returning integral value or error
--
-- > λ:> readBo "༣༢༠༥"
-- > Just 3205
readBo :: (Integral a) => String -> Maybe a
readBo = rightToMaybe . readBoV

-- | Return verbose errors.
--
-- @since 0.1.2.0
readBoV :: (Integral a) => String -> Either (ParseErrorBundle String Void) a
readBoV = fmap fromIntegral . runParser (parseNumber :: Parser Integer) ""

-- | Parse Tibetan numerals, returning a positive integer
parseNumber :: (Integral a, MonadParsec e s m, Token s ~ Char) => m a
parseNumber = do
    digits <- reverse <$> some parseNumeral
    (pure . (`div` 10) $ foldr ((*10) .* (+)) 0 digits) <?> "tibetan integer"

-- | Parse a single digit
parseNumeral :: (Integral a, MonadParsec e s m, Token s ~ Char) => m a
parseNumeral = foldr (<|>) (parseDigit '༠' 0) $ zipWith parseDigit "༠༡༢༣༤༥༦༧༨༩" [0..9]

-- | m a given char as a given integer
parseDigit :: (Integral a, MonadParsec e s m, Token s ~ Char) => Char -> a -> m a
parseDigit c i = do
    char c
    pure i
