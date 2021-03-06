{-# LANGUAGE OverloadedStrings #-}

-- | Module to display positive integral values as strings
module TextShow.Data.Integral.Tibetan ( builderBo
                                      , showBo) where

import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder, fromLazyText, toLazyText)

-- | Display positive integer as a `String`
--
-- > λ:> showBo 312
-- > Just "༣༡༢"
showBo :: Integer -> Maybe String
showBo = fmap (TL.unpack.toLazyText) . builderBo

-- | show a positive intgeger as text, return `Nothing` if that doesn't work
builderBo :: (Integral a) => a -> Maybe Builder
builderBo int
    | int >= 0  = Just . fromLazyText $ foldr (.) id
        (zipWith TL.replace
            (fmap showT [0..9])
            ["༠","༡","༢","༣","༤","༥","༦","༧","༨","༩"])
        (showT (fromIntegral int :: Integer))
    | otherwise = Nothing

showT = TL.pack . show
