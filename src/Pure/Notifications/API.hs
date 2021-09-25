{-# language TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Pure.Notifications.API where

import Pure.Elm (View,Time)
import Pure.WebSocket as WS

mkMessage "Notify" 
  [t|(View,Time)|]

api = WS.api msgs reqs
  where
    msgs = notify <:> WS.none
    reqs = WS.none 