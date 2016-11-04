{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget
    ( toAction
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event
import qualified Event.Keyboard            as Keyboard
import           Event.Mouse               (EventWidget (..))
import qualified Event.Mouse               as Mouse
import qualified Event.Widget              as WE
import           Object.Widget             (UIHandlers, WidgetId, widget, widgetId)
import qualified Object.Widget             as Widget
import           Object.Widget.Port        ()
import           Reactive.Commands.Command (Command)
import           Reactive.State.Camera     (Camera)
import qualified Reactive.State.Camera     as Camera
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import           UI.Handlers               (widgetHandlers)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse jsState event) = Just $ do
    handleMouseOverOut jsState event
    handleMouseDrag    jsState event
    handleMouseGeneric jsState event
    handleMouseClick   jsState event
toAction (Keyboard jsState event) = Just $ handleKeyboardGeneric jsState event
toAction (Widget (WE.WidgetEvent wid payload)) = Just $ handleWidgetCustom wid payload
toAction _ = Nothing

handleMouseGeneric :: JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseGeneric jsState event@(Mouse.Event eventType absPos _ _ (Just (EventWidget wid mat scene))) = do
    mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM wid
    withJust mayFile $ \file -> do
        camera <- use $ Global.camera . Camera.camera
        let pos = absPosToRel scene camera mat (fromIntegral <$> absPos)
        let updatedEvent = event & Mouse.position .~ pos
        let handlers = widgetHandlers (file ^. widget)
        case eventType of
            Mouse.Moved       -> (handlers ^. Widget.mouseMove)     updatedEvent jsState wid
            Mouse.Pressed     -> (handlers ^. Widget.mousePressed)  updatedEvent jsState wid
            Mouse.Released    -> (handlers ^. Widget.mouseReleased) updatedEvent jsState wid
            Mouse.Clicked     -> (handlers ^. Widget.click)         updatedEvent jsState wid
            Mouse.DblClicked  -> (handlers ^. Widget.dblClick)      updatedEvent jsState wid
            _                 -> return ()
handleMouseGeneric _ _ = return ()

runOverHandler :: JSState -> (UIHandlers Global.State -> (JSState -> WidgetId -> Command Global.State ())) -> Command Global.State ()
runOverHandler jsState handler = do
    mayWidgetOver <- use $  Global.uiRegistry . UIRegistry.widgetOver
    withJust mayWidgetOver $ \widgetOver -> do
        mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM widgetOver
        withJust mayFile $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            handler handlers jsState widgetOver

handleMouseOut :: JSState -> Command Global.State ()
handleMouseOut jsState = do
    runOverHandler jsState $ view Widget.mouseOut
    Global.uiRegistry . UIRegistry.widgetOver .= Nothing

handleMouseOverOut ::  JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseOverOut jsState (Mouse.Event Mouse.Moved _ _ _ Nothing) = handleMouseOut jsState
handleMouseOverOut jsState (Mouse.Event Mouse.Moved _ _ _ (Just (EventWidget wid _ _))) = do
    widgetOver <- use $ Global.uiRegistry . UIRegistry.widgetOver
    when (widgetOver /= Just wid) $ do
        handleMouseOut jsState
        Global.uiRegistry . UIRegistry.widgetOver ?= wid
        runOverHandler jsState $ view Widget.mouseOver

handleMouseOverOut _ _ = return ()

absPosToRel :: Widget.SceneType -> Camera -> [Double] -> Vector2 Double -> Vector2 Double
absPosToRel Widget.HUD       _      mat pos = Widget.sceneToLocal pos          mat
absPosToRel Widget.Workspace camera mat pos = Widget.sceneToLocal workspacePos mat where
    workspacePos = Camera.screenToWorkspace camera (round <$> pos)

handleKeyboardGeneric :: JSState -> Keyboard.Event -> Command Global.State ()
handleKeyboardGeneric jsState (Keyboard.Event eventType ch mods) = do
    mayFocusedWidget <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    withJust mayFocusedWidget $ \focusedWidget -> do
        mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM focusedWidget
        withJust mayFile $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            case eventType of
                Keyboard.Up       -> (handlers ^. Widget.keyUp)       ch mods jsState focusedWidget
                Keyboard.Down     -> (handlers ^. Widget.keyDown)     ch mods jsState focusedWidget
                Keyboard.Press    -> (handlers ^. Widget.keyPressed)  ch mods jsState focusedWidget

handleMouseDrag ::  JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseDrag jsState (Mouse.Event Mouse.Moved absPos button keymods _) = do
    mayDragState <- use $ Global.uiRegistry . UIRegistry.dragState
    withJust mayDragState $ \dragState -> do
        let stillDragging = dragState ^. Widget.button == button
        if stillDragging then dragInProgress jsState dragState absPos keymods
                         else stopDrag       jsState

handleMouseDrag jsState (Mouse.Event Mouse.Released _ _ _ _) = stopDrag jsState
handleMouseDrag _ _ = return ()

dragInProgress :: Integral a => JSState -> Widget.DragState -> Vector2 a -> Keyboard.KeyMods -> Command Global.State ()
dragInProgress jsState dragState absPos keymods = do
    mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM $ dragState ^. Widget.widgetId
    camera <- use $ Global.camera . Camera.camera
    let pos = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. Widget.widgetMatrix) (fromIntegral <$> absPos)

    zoom (Global.uiRegistry . UIRegistry.dragState . _Just) $ do
        Widget.keyMods .= keymods
        prevPos' <- use Widget.previousPos
        Widget.previousPos .= prevPos'
        Widget.currentPos .= pos

    withJust mayFile $ \file -> do
        let handlers = widgetHandlers (file ^. widget)
        (handlers ^. Widget.dragMove) dragState jsState (dragState ^. Widget.widgetId)

stopDrag :: JSState -> Command Global.State ()
stopDrag jsState = do
    mayDragState <- use $ Global.uiRegistry . UIRegistry.dragState
    withJust mayDragState $ \dragState -> do
        mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM $ dragState ^. widgetId
        withJust mayFile $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            (handlers ^. Widget.dragEnd) dragState jsState (dragState ^. widgetId)

        Global.uiRegistry . UIRegistry.dragState .= Nothing

handleMouseClick :: JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseClick _ (Mouse.Event Mouse.Pressed _ Mouse.LeftButton _ (Just (EventWidget wid _ _))) =
    Global.uiRegistry . UIRegistry.mouseDownWidget ?= wid
handleMouseClick jsState event@(Mouse.Event Mouse.Released _ Mouse.LeftButton _ (Just (EventWidget wid _ _))) = do
    previousWidget <- use $ Global.uiRegistry . UIRegistry.mouseDownWidget
    when (previousWidget == Just wid) $ do
        let modEvent = event & Mouse.tpe .~ Mouse.Clicked
        handleMouseGeneric jsState modEvent
    Global.uiRegistry . UIRegistry.mouseDownWidget .= Nothing

handleMouseClick _ _ = return ()

handleWidgetCustom :: WidgetId -> WE.Payload -> Command Global.State ()
handleWidgetCustom wid payload = do
    mayFile <- zoom Global.uiRegistry $ UIRegistry.lookupM wid
    withJust mayFile $ \file -> do
        let handlers = widgetHandlers (file ^. widget)
        (handlers ^. Widget.widgetCustom) payload wid
