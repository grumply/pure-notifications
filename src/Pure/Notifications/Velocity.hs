{-# language PatternSynonyms, DerivingStrategies, DerivingVia, ViewPatterns #-}
module Pure.Notifications.Velocity
  ( Velocity(..)
  , pattern V
  ) where

import Pure.Data.Time

import Pure.Notifications.Event

newtype Velocity = Velocity Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

toVelocity :: Event -> Event -> Velocity
toVelocity e0 e1 = Velocity $ d / delta_t
  where 
    delta_x = ex e1 - ex e0
    delta_y = ey e1 - ey e0
    d = sqrt (delta_x ^ (2 :: Int) + delta_y ^ (2 :: Int))
    Millis delta_t 
      | et e0 == et e1 = 1
      | otherwise = fromTime $ et e1 - et e0

fromVelocity :: Velocity -> (Event,Event)
fromVelocity (Velocity v) = (Event 0 0 0,Event z z 1)
  where
    z = sqrt (v ^ (2 :: Int) / 2)

pattern V :: Event -> Event -> Velocity
pattern V e0 e1 <- (fromVelocity -> (e0,e1)) where
  V e0 e1 = toVelocity e0 e1

