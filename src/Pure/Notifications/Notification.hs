{-# language AllowAmbiguousTypes, PatternSynonyms, LambdaCase, BlockArguments, RankNTypes, TypeApplications, DataKinds, RecordWildCards, ViewPatterns, NamedFieldPuns, ScopedTypeVariables, PostfixOperators, OverloadedStrings, PolyKinds #-}
module Pure.Notifications.Notification
  ( Notification (..),
    notification,
    dismissAll
  )
where

import Pure.Notifications.Acceleration (pattern A)
import Pure.Notifications.Between (between)
import Pure.Notifications.Event (mouseEvent,touchEvent,Event)
import Pure.Notifications.Offset (pattern Ox,pattern OffsetX)
import Pure.Notifications.Velocity (pattern V)

import Pure.Data.Txt as Txt (tail)
import Pure.Elm hiding (pattern A,pattern Start,between,duration,shrink,start)

import Control.Concurrent (forkIO)
import Control.Monad (void,when)
import qualified Data.List as List
import Data.Unique (hashUnique,newUnique)
import Prelude hiding (max,min,all)
import qualified Prelude

{-
Implementation based on ideas in the MIT-licensed:
  https://github.com/Dogfalo/materialize/blob/v1-dev/js/toasts.js
-}
data DismissAll = DismissAll

dismissAll :: IO ()
dismissAll = publish DismissAll

data Notification = Notification
  { notificationId :: Int,
    duration :: Time,
    content :: IO () -> View,
    dismiss :: IO ()
  }

data Model
  = Initial
  | Resting
  | Shrinking
  | Flattening
  | Dismissing
      { _transform :: Txt,
        _opacity :: Txt
      }
  | Resetting
      { _transform :: Txt,
        _opacity :: Txt
      }
  | Panning
      { _events :: [Event],
        _transform :: Txt,
        _opacity :: Txt
      }

data InputType = Touch | Mouse

toEvent :: InputType -> Evt -> Event
toEvent ty
  | Touch <- ty = touchEvent
  | otherwise   = mouseEvent

data Msg
  = Startup
  | Reinit
  | Shrink
  | Dismiss
  | Flatten
  | Start
  | Drag InputType Evt
  | Stop InputType Evt

type Update = Elm Msg => Notification -> Model -> IO Model

notification :: Notification -> View
notification = run (Applet [Startup] [] [] (pure Initial) update view)

update :: Msg -> Update
update = \case
  Startup  -> startup 
  Reinit   -> reinit
  Dismiss  -> dismiss'
  Shrink   -> shrink
  Flatten  -> flatten
  Start    -> start
  Drag i e -> drag i e
  Stop i e -> stop i e
  
startup :: Update
startup Notification { duration } _ = do
  subscribeWith (\DismissAll -> Dismiss)

  when (duration > 0) do
    void do
      forkIO do
        delay duration
        command Dismiss

  pure Initial
    
reinit :: Update
reinit _ _ = pure Resting

dismiss' :: Update
dismiss' _ _ = pure (Dismissing "" "1")

shrink :: Update
shrink _ _ = pure Shrinking

flatten :: Update
flatten _ _ = pure Flattening

start :: Update
start _ _ =
  let 
    -- ev = if isTouch then touchEvent evt else mouseEvent evt
    _opacity = "1"
    _transform = ""
    _events = []
  in 
    pure Panning {..}

drag :: InputType -> Evt -> Update
drag ty evt _ = \case 
  Panning {..} -> let es = toEvent ty evt : _events in pure (analyze es Dismissing (Panning es))
  mdl -> pure mdl
    
stop :: InputType -> Evt -> Update
stop ty evt _ = \case
  Panning {..} -> pure (analyze ( toEvent ty evt : _events ) Dismissing Resetting)
  mdl -> pure mdl

-- A tuned analysis of a drag event, either touch or mouse, to determine 
-- if the dismissal velocity, acceleration, or displacement thresholds 
-- have been exceeded.
analyze :: [Event] -> (Txt -> Txt -> a) -> (Txt -> Txt -> a) -> a
analyze events dismissing other
  | d > minD
  , v:_ <- between V events
  , abs v > maxV 
  = dismissing translation opacity

  | d > minD
  , a:_ <- between A events
  , abs a > maxA
  = dismissing translation opacity

  | d > maxD
  = dismissing translation opacity

  | otherwise 
  = other translation opacity 
  where
    -- opacity based on displacement, clamped to a minimum of 10%
    opacity = dec (1 - Prelude.min 0.9 (d / 250))
  
    -- translation based on mouse displacement offset clamped to 250 pxs either left or right
    translation = translateX (pxs @Int $ round $ Prelude.max (-250) (Prelude.min 250 x))
    
    -- relative, x, and absolute, d, mouse displacement offset in pxs
    OffsetX x@(abs -> d) = Ox (last events) (Prelude.head events)

    (maxV,maxA) = (2,0.5)
    (minD,maxD) = (80,180)

view :: Elm Msg => Notification -> Model -> View
view (Notification i _ cnt dismiss) mdl =
  let 
    listeners =
        OnTouchStart (const (command Start))
      . OnTouchMove  (command . Drag Touch)
      . OnTouchEnd   (command . Stop Touch)
      . OnMouseDown  (const (command Start))
      . OnMouseMove  (command . Drag Mouse)
      . OnMouseUp    (command . Stop Mouse)

    docListeners =
        OnDoc "touchmove" (command . Drag Touch)
      . OnDoc "touchend"  (command . Stop Touch)
      . OnDoc "mousemove" (command . Drag Mouse)
      . OnDoc "mouseup"   (command . Stop Mouse)

    props = mdl & \case
      Initial        -> state @Initial    listeners
      Resting        -> state @Resting    listeners
      Shrinking      -> state @Shrinking  transitioning 
      Flattening     -> state @Flattening transitioning  
      Dismissing t o -> state @Dismissing (animating Flatten t o)
      Resetting t o  -> state @Resetting  (animating Reinit t o)
      Panning _ t o  -> state @Panning    (listeners . docListeners . styles t o)
      where
        state :: forall theme. Theme theme => (View -> View) -> View -> View
        state = (Themed @theme .)

        transitioning :: View -> View
        transitioning = On "transitionend" (\_ -> dismiss)
          
        animating :: Msg -> Txt -> Txt -> View -> View
        animating nxt t o = On "animationend" (\_ -> command nxt) . styles t o
        
        styles t o = Style transform t . Style opacity o

   in 

     Div <| Themed @Notification . props |> 
      [ cnt (command Shrink)
      ]


instance Theme Initial
instance Theme Resting
instance Theme Dismissing
instance Theme Resetting
instance Theme Flattening
instance Theme Shrinking
instance Theme Panning

instance Theme Notification where
  theme c = do
    let nm = Txt.tail c
    is c do
      opacity        =: 1
      max-height     =: 100vmin
      overflow       =: visible
      pointer-events =: all
      will-change    =: elems [max-height, opacity, transform]

      is (subtheme @Initial) do
        transform - origin =: top
        animation =* [nm <> "_enter", 0.25s, linear, "forwards"]

      is (subtheme @Dismissing) do
        animation =* [nm <> "_exit", 0.1s, linear, "forwards"]

      is (subtheme @Resetting) do
        animation =* [nm <> "_reset", 0.2s, linear, "forwards"]

      is (subtheme @Flattening) do
        max-height =: 0
        opacity =: 0
        transition =* [ max-height, 0.1s, cubicBezier(0,1.05,0.8,0.8) ]

      is (subtheme @Shrinking) do
        opacity =: 0
        max-height =: 0
        transition
          =* [ max-height,
               0.2 s,
               cubicBezier(0, 1, 0.8, 0.8),
               ", ",
               opacity,
               0.2s,
               linear
             ]

    atKeyframes (nm <> "_enter") do
      is (0 %) do
        max-height =: 0
        opacity    =: 0
        transform  =: translateY ((-130) %)

      is (100 %) do
        max-height =: 100vmin
        opacity    =: 1
        transform  =: translateY (0%)

    atKeyframes (nm <> "_exit") do
      -- from the current opacity and translateX
      --
      -- applied when a Panning action triggers a
      -- dismiss
      is (100 %) do
        opacity   =: 0
        transform =: translateX (130%)

    atKeyframes (nm <> "_reset") do
      -- from the current opacity and translateX
      --
      -- applied when a Panning action fails to
      -- dismiss
      is (100 %) do
        opacity   =: 1
        transform =: translateX (0%)
