
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.TabbedWithTray where
import Control.Monad
import           XMonad                         ( Message
                                                , LayoutClass
                                                , runLayout
                                                , handleMessage
                                                , fromMessage
                                                , WorkspaceId
                                                , Rectangle
                                                , X
                                                , SomeMessage
                                                , Window
                                                , get
                                                )
import           XMonad.StackSet                ( Workspace(Workspace), StackSet(..), stack, Stack(..), Screen(..) )
import           XMonad.Core           (XState(..))
import           Control.Lens                   ( (^.) 
                                                , Lens'
                                                , (?~)
                                                , (.~)
                                                , lens
                                                , (&)
                                                )
import qualified XMonad.Operations as X
import XMonad.Layout.Decoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Simplest
import XMonad.Config.Prime()

import XMonad.Lens


--- Message Types

data MsgDividerRight = MsgDividerRight 
instance Message MsgDividerRight

data MsgPutWindowInTray = MsgPutWindowInTray 
instance Message MsgPutWindowInTray

data MsgClearTray = MsgClearTray
instance Message MsgClearTray

data MsgToggleFocus = MsgToggleFocus
instance Message MsgToggleFocus

type TabbedT a = ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest a

data TabbedTrayState a = TabbedTrayState [a] [a]
  deriving (Show, Read)


data TabbedTrayLayout a = TabbedTrayLayout
   { tabbedTrayWindows :: Maybe a 
   , tabbedSubStack :: Maybe (Stack a)
   , tabbedLayout :: TabbedT a
   }
  deriving (Show, Read)

tabbedWithTray :: TabbedTrayLayout Window
tabbedWithTray = TabbedTrayLayout 
   { tabbedTrayWindows = Nothing
   , tabbedSubStack = Nothing
   , tabbedLayout = simpleTabbed
   }

lTabbedTrayWindows :: Lens' (TabbedTrayLayout a) (Maybe a)
lTabbedTrayWindows = lens tabbedTrayWindows (\t n -> t { tabbedTrayWindows = n })

lTabbedSubStack :: Lens' (TabbedTrayLayout a) (Maybe (Stack a))
lTabbedSubStack = lens tabbedSubStack (\t n -> t { tabbedSubStack = n })

lTabbedLayout :: Lens' (TabbedTrayLayout a) (TabbedT a)
lTabbedLayout = lens tabbedLayout (\t n -> t { tabbedLayout = n })

removeFromStack :: Eq a => a -> Stack a -> Maybe (Stack a)
removeFromStack a Stack {focus, up, down} =
    if focus == a then
        case up of
            x:xs -> Just $ Stack {focus=x, up=xs, down}
            [] -> 
                case down of
                    x:xs -> Just $ Stack {focus=x, up=[], down=xs}
                    [] -> Nothing
    else Just $ Stack {focus, up = filter (/=a) up, down = filter (/=a) down}


runLayoutTTL
    :: Workspace WorkspaceId (TabbedTrayLayout Window) Window
    -> Rectangle
    -> X ([(Window, Rectangle)], Maybe (TabbedTrayLayout Window))
runLayoutTTL ws rect = do
    let layout = ws ^. lWSLayout
    let stack = ws ^. lWSStack

    let trayWindow = layout ^. lTabbedTrayWindows
    let tabbedLayout = layout ^. lTabbedLayout

    let (tabbedRect, tabbedStack, tray) =
         case trayWindow of
            Nothing -> (rect, stack, [])
            Just window -> let totalWidth = rect ^. lRectWidth
                               leftW = 2 * (totalWidth `div` 3)
                               rightW = totalWidth - leftW
                               rect' = rect & lRectWidth .~ leftW
                               stack' = join $ fmap (removeFromStack window) stack

                               trayRect = rect & lRectX .~ (rect ^. lRectX) + fromIntegral leftW
                                               & lRectWidth .~ rightW
                               trayTupple = (window, trayRect)
                            in (rect', stack', [trayTupple])

    let tabbedFocused =
         case tabbedStack of
            Nothing -> Nothing
            Just Stack {focus} -> Just focus

    let innerWS = ws & lWSLayout .~ tabbedLayout
                     & lWSStack .~ tabbedStack
    (windowList, newState) <- runLayout innerWS tabbedRect
    return $ case newState of
        Nothing -> do
            let newLayout = layout & lTabbedSubStack .~ tabbedStack
            (windowList ++ tray, Just newLayout)
        Just newTabbedState -> do
            let newLayout = layout & lTabbedLayout .~ newTabbedState
                                   & lTabbedSubStack .~ tabbedStack 
            (windowList ++ tray, Just newLayout)

toggleFocus :: TabbedTrayLayout Window -> X ()
toggleFocus TabbedTrayLayout { tabbedTrayWindows, tabbedSubStack} =
    case tabbedTrayWindows of
        Nothing -> return ()
        Just a -> do
            stack <- getCurrentStack
            case stack of
                Nothing -> return ()
                Just stack' -> do
                    let Stack {focus = currentFocus} = stack'
                    if currentFocus /= a
                          then X.focus a
                          else do
                            case tabbedSubStack of
                                Nothing -> return ()
                                Just Stack {focus = tabbedFocus} ->
                                    X.focus tabbedFocus



getCurrentStack :: X (Maybe (Stack Window))
getCurrentStack = do
    XState {windowset = StackSet { current = Screen { workspace = Workspace { stack=stack }}}} <- get
    return stack

handleMessageTTL :: TabbedTrayLayout Window
    -> SomeMessage
    -> X (Maybe (TabbedTrayLayout Window))
handleMessageTTL _ msg | Just MsgDividerRight <- fromMessage msg =
    return Nothing
handleMessageTTL l msg | Just MsgPutWindowInTray <- fromMessage msg = do
    stack <- getCurrentStack
    case stack of
        Nothing -> return Nothing
        Just stack' -> do
            let Stack {focus} = stack'
            return $ Just $ l & lTabbedTrayWindows ?~ focus
handleMessageTTL l msg | Just MsgClearTray <- fromMessage msg = 
    return $ Just $ l & lTabbedTrayWindows .~ Nothing
handleMessageTTL l msg | Just MsgToggleFocus <- fromMessage msg = do
    toggleFocus l
    return Nothing
handleMessageTTL l msg = do
    maybeLayout <- handleMessage (l ^. lTabbedLayout) msg
    return $ fmap (\inner -> l & lTabbedLayout .~ inner) maybeLayout

instance LayoutClass TabbedTrayLayout Window where
    runLayout = runLayoutTTL
    handleMessage = handleMessageTTL

