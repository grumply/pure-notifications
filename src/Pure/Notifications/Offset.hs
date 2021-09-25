{-# language PatternSynonyms, DerivingStrategies, DerivingVia, ViewPatterns #-}
module Pure.Notifications.Offset
  ( OffsetX(..)
  , OffsetY(..)
  , pattern Ox
  , pattern Oy
  ) where

import Pure.Notifications.Event

newtype OffsetX = OffsetX Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

toOffsetX :: Event -> Event -> OffsetX
toOffsetX e0 e1 = OffsetX (ex e1 - ex e0)

fromOffsetX :: OffsetX -> (Event,Event)
fromOffsetX (OffsetX x) = (Event 0 0 0,Event z z 1)
  where
    z = sqrt (x ^ (2 :: Int) / 2)

pattern Ox :: Event -> Event -> OffsetX
pattern Ox e0 e1 <- (fromOffsetX -> (e0,e1)) where
  Ox e0 e1 = toOffsetX e0 e1

newtype OffsetY = OffsetY Double
  deriving stock (Eq,Ord,Show)
  deriving (Num,Fractional,Real,RealFrac) via Double

toOffsetY :: Event -> Event -> OffsetY
toOffsetY e0 e1 = OffsetY (ey e1 - ey e0)

fromOffsetY :: OffsetY -> (Event,Event)
fromOffsetY (OffsetY y) = (Event 0 0 0,Event z z 1)
  where
    z = sqrt (y ^ (2 :: Int) / 2)

pattern Oy :: Event -> Event -> OffsetY
pattern Oy e0 e1 <- (fromOffsetY -> (e0,e1)) where
  Oy e0 e1 = toOffsetY e0 e1
