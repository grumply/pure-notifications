{-# language CPP #-}
module Pure.Notifications.Proportions
  ( w, h, d
  ) where

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.clientWidth" w :: Double

foreign import javascript unsafe
  "window.clientHeight" h :: Double
#else
w :: Double
w = 1920
h :: Double
h = 1200
#endif

d :: Double
d = sqrt (w ^ (2 :: Int) + h ^ (2 :: Int))

