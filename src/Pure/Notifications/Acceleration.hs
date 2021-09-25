{-# language PatternSynonyms, DerivingStrategies, DerivingVia, ViewPatterns #-}
module Pure.Notifications.Acceleration
  ( Acceleration(..)
  , pattern A
  ) where

import Pure.Data.Time

import Pure.Notifications.Event

newtype Acceleration = Acceleration Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

toAcceleration :: Event -> Event -> Acceleration
toAcceleration e0 e1 = Acceleration (2 * d / delta_t ^ (2 :: Int))
  where
    delta_x = ex e1 - ex e0
    delta_y = ey e1 - ey e0
    d = sqrt (delta_x ^ (2 :: Int) + delta_y ^ (2 :: Int)) 
    Millis delta_t 
      | et e0 == et e1 = 1
      | otherwise = fromTime $ et e1 - et e0

fromAcceleration :: Acceleration -> (Event,Event)
fromAcceleration (Acceleration a) = (Event 0 0 0,Event z z 1)
  where
    z = sqrt (a ^ (2 :: Int) / 2) / 2

pattern A :: Event -> Event -> Acceleration
pattern A e0 e1 <- (fromAcceleration -> (e0,e1)) where
  A e0 e1 = toAcceleration e0 e1
