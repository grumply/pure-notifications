{-# language DerivingStrategies, ViewPatterns, OverloadedStrings #-}
module Pure.Notifications.Event
  ( Event(..)
  , boundingEvent
  , mouseEvent
  , touchEvent
  , averageEvent
  ) where

import Pure.Elm hiding (ex)
import Pure.Data.Lifted (BoundingRect(..),getBoundingRect)

import Data.Coerce
import Data.Maybe

data Event = Event
  { ex :: {-# UNPACK #-}!Double
  , ey :: {-# UNPACK #-}!Double
  , et :: {-# UNPACK #-}!Time 
  } deriving stock (Show,Eq,Ord)

boundingEvent :: Evt -> IO Event
boundingEvent evt = do
  br <- getBoundingRect (coerce $ evtTarget evt)
  now <- time
  pure $! Event (brLeft br) (brTop br) now

mouseEvent :: Evt -> Event
mouseEvent (evtObj -> e) =
  fromMaybe (error "Pure.Notifications.Event.mouseEvent: fromMaybe got Nothing") $! do
    t <- e .# "timeStamp"
    x <- e .# "pageX"
    y <- e .# "pageY"
    pure (Event x y (Milliseconds t 0))

touchEvent :: Evt -> Event
touchEvent (evtObj -> e) =
  fromMaybe (error "Pure.Notifications.Event.touchEvent: fromMaybe got Nothing") $! do
    t  <- e  .# "timeStamp"
    ts <- e  .# "changedTouches"
    te <- ts .# "0"
    x  <- te .# "pageX"
    y  <- te .# "pageY"
    pure (Event x y (Milliseconds t 0))

averageEvent :: Event -> Event -> Event
averageEvent e0 e1 = Event x y t
  where
    t = (et e1 - et e0) / 2
    x = (ex e1 - ex e0) / 2
    y = (ey e1 - ey e0) / 2
