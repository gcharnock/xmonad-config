
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
import           Control.Lens                   ( Lens
                                                , Lens'
                                                , (^.)
                                                , (?~)
                                                , (.~)
                                                , lens
                                                , (&)
                                                )

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

type TabbedT a = ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest a

data TabbedTrayState a = TabbedTrayState [a] [a]
  deriving (Show, Read)


data TabbedTrayLayout a = TabbedTrayLayout
   { tabbedTrayWindows :: Maybe a 
   , tabbedLayout :: TabbedT a
   }
  deriving (Show, Read)

tabbedWithTray :: TabbedTrayLayout Window
tabbedWithTray = TabbedTrayLayout 
   { tabbedTrayWindows = Nothing
   , tabbedLayout = simpleTabbed
   }

lTabbedTrayWindows :: Lens' (TabbedTrayLayout a) (Maybe a)
lTabbedTrayWindows = lens tabbedTrayWindows (\t n -> t { tabbedTrayWindows = n })

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
                               trayWindow = (window, trayRect)
                            in (rect', stack', [trayWindow])

    let innerWS = ws & lWSLayout .~ tabbedLayout
                     & lWSStack .~ tabbedStack
    (windowList, newState) <- runLayout innerWS tabbedRect
    return $ case newState of
        Nothing -> (windowList ++ tray, Nothing)
        Just newTabbedState ->
             (windowList ++ tray, Just $ layout & lTabbedLayout .~ newTabbedState)


handleMessageTTL :: TabbedTrayLayout Window
    -> SomeMessage
    -> X (Maybe (TabbedTrayLayout Window))
handleMessageTTL l msg | Just MsgDividerRight <- fromMessage msg =
    return Nothing
handleMessageTTL l msg | Just MsgPutWindowInTray <- fromMessage msg = do
    XState {windowset = StackSet { current = Screen { workspace = Workspace { stack=stack }}}} <- get
    case stack of
        Nothing -> return Nothing
        Just stack' -> do
            let Stack {focus} = stack'
            return $ Just $ l & lTabbedTrayWindows ?~ focus
handleMessageTTL l msg | Just MsgClearTray <- fromMessage msg = do
    return $ Just $ l & lTabbedTrayWindows .~ Nothing
handleMessageTTL l msg = do
    maybeLayout <- handleMessage (l ^. lTabbedLayout) msg
    return $ fmap (\inner -> l & lTabbedLayout .~ inner) maybeLayout

instance LayoutClass TabbedTrayLayout Window where
    runLayout = runLayoutTTL
    handleMessage = handleMessageTTL

