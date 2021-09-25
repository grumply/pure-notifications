{-# language PatternSynonyms, ViewPatterns, DerivingStrategies, DerivingVia #-}
module Pure.Notifications.Direction
  ( Direction(..)
  , pattern D
  ) where

import Pure.Notifications.Event
import Pure.Notifications.Proportions

newtype Direction = Direction Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

toDirection :: Event -> Event -> Direction
toDirection e0 e1 = Direction d
  where
    delta_y = ey e1 - ey e0
    delta_x = ex e1 - ex e0
    d = atan (delta_y / delta_x)

fromDirection :: Direction -> (Event,Event)
fromDirection (Direction dir) = (Event 0 0 0,Event x y 1)
  where
    x = d * cos dir
    y = d * sin dir

pattern D :: Event -> Event -> Direction
pattern D e0 e1 <- (fromDirection -> (e0,e1)) where
  D e0 e1 = toDirection e0 e1