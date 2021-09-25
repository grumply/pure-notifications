{-# language PatternSynonyms, ViewPatterns, DerivingStrategies, DerivingVia #-}
module Pure.Notifications.Interval
  ( Interval(..)
  , pattern I
  ) where

import Pure.Notifications.Event
import Pure.Notifications.Proportions

import Pure.Elm hiding (ex,I)

newtype Interval = Interval Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

d2i :: Double -> Interval
d2i x = Interval (x / d)

i2d :: Interval -> Double
i2d (Interval i) = i * d

fromInterval :: Interval -> (Event,Event)
fromInterval i = (Event 0 0 0,Event x x 1)
  where
    z = i2d i
    x = sqrt (z ^ (2 :: Int) / 2)

toInterval :: Event -> Event -> Interval
toInterval e0 e1 = Interval $ sqrt (delta_x ^ (2 :: Int) + delta_y ^ (2 :: Int)) / delta_t / d
  where
    delta_x = ex e1 - ex e0
    delta_y = ey e1 - ey e0
    Millis delta_t 
      | et e0 == et e1 = 1
      | otherwise = fromTime $ (et e1 - et e0)

pattern I :: Event -> Event -> Interval
pattern I e0 e1 <- (fromInterval -> (e0,e1)) where
  I e0 e1 = toInterval e0 e1