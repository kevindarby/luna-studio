{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Generic where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           Utils.Vector
import           GHCJS.Types         (JSVal)
import           Object.Widget hiding (setPosition)
import qualified Data.JSString as JSString
import qualified Event.Mouse as Mouse
import           Object.UITypes (WidgetId)
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

import           Reactive.Commands.Command (Command, ioCommand, performIO)
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified Reactive.State.Global as Global
import qualified Reactive.State.Camera as Camera
import           UI.Widget (UIWidget, UIContainer, GenericWidget(..))
import qualified UI.Registry as UIR


foreign import javascript unsafe "$1.mesh.position.x = $2; $1.mesh.position.y = $3"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "app.removeWidget($1)"
    removeWidget :: Int -> IO ()

setWidgetPosition :: UIWidget a => Vector2 Double -> a -> IO ()
setWidgetPosition (Vector2 x y) widget = setWidgetPosition' (pToJSVal widget) x y

updatePosition :: (IsDisplayObject b) => WidgetFile b -> Command UIRegistry.State ()
updatePosition file = performIO $ do
    let position = file ^. widget . widgetPosition
    w <- UIR.lookup $ file ^. objectId :: IO (GenericWidget)
    setWidgetPosition position w

updatePosition' :: WidgetId -> Vector2 Double -> IO ()
updatePosition' id pos = do
        w <- UIR.lookup $ id :: IO (GenericWidget)
        setWidgetPosition pos w

takeFocus :: a -> WidgetId -> Command Global.State ()
takeFocus _ id = Global.uiRegistry . UIRegistry.focusedWidget ?= id

startDrag :: Mouse.Event' -> WidgetId -> Command Global.State ()
startDrag event@(Mouse.Event eventType pos button keymods (Just (Mouse.EventWidget widgetId mat scene))) id = do
    camera <- use $ Global.camera . Camera.camera
    Global.uiRegistry . UIRegistry.dragState ?= DragState widgetId mat scene button keymods pos pos pos
