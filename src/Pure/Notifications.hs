{-# language LambdaCase, BlockArguments, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, PartialTypeSignatures #-}
module Pure.Notifications
  ( Notifications(..),
    local,
    notifications,
    notify,
    dismissAll
  )
where

import Pure.Notifications.Notification (notification,Notification(..),dismissAll)
import qualified Pure.Notifications.API as API

import Pure.Data.Render ()
import Pure.Data.Txt as Txt (tail)
import Pure.Elm hiding (A, Start, between, duration)
import Pure.WebSocket as WS hiding (remove)

import Control.Concurrent ( forkIO )
import Control.Monad ( void )
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List as List
import Data.Unique ( hashUnique, newUnique )
import Prelude hiding (max, min, all)
import qualified Prelude

data Notifications = Notifications
  { remote         :: Maybe WebSocket
  , toNotification :: View -> (IO () -> View)
  }

data Model = Model
  { stack :: [(Int, Notification)]
  }

data Msg
  = Startup
  | Notify Int Notification
  | Remove Int

local :: View
local = notifications (Notifications Nothing (\v _ -> v))

notifications :: Notifications -> View
notifications = run (Applet [Startup] [] [] (pure (Model [])) update view)

type Update = (Elm Msg) => Notifications -> Model -> IO Model

update :: Msg -> Update
update = \case
  Startup    -> startup
  Remove i   -> remove i
  Notify i n -> notify' i n

startup :: Update
startup nots@Notifications { remote } mdl = do
  subscribe
  for_ remote (`enact` (notificationsEndpoints nots))
  pure mdl

remove :: Int -> Update 
remove i _ mdl =
  pure mdl {stack = List.filter ((/= i) . fst) (stack mdl) }

notify' :: Int -> Notification -> Update
notify' i n _ mdl = do
  pure mdl {stack = (i, n) : stack mdl}

type Render = (Elm Msg) => Model -> View

view :: Notifications -> Render
view env mdl =
  Keyed Div <| Themed @Notifications |#> 
    ( ((-1),if Prelude.not (Prelude.null (stack mdl)) then Button <| OnClick (const dismissAll) else Null)
    : [ (k, notification n)
      | (k, n) <- stack mdl
      ]
    )

instance Theme Notifications where
  theme c = do
    is c do
      pointer-events =: all

notificationsEndpoints :: Notifications -> _ 
notificationsEndpoints notifications = Endpoints API.api msgs reqs
  where
    msgs = handleNotify notifications <:> WS.none
    reqs = WS.none

    handleNotify :: Notifications -> MessageHandler API.Notify
    handleNotify Notifications { toNotification } = awaiting do
      (n,t) <- acquire
      liftIO (notify t (toNotification n))
 
notify :: Time -> (IO () -> View) -> IO ()
notify duration content = do
  notificationId <- hashUnique <$> newUnique
  let dismiss = publish (Remove notificationId)
  publish $ Notify notificationId Notification {..}

