
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.ToggleLayout where

import           XMonad                         ( Message
                                                , LayoutClass
                                                , runLayout
                                                , handleMessage
                                                , fromMessage
                                                , Full(Full)
                                                , WorkspaceId
                                                , Rectangle
                                                , X
                                                , SomeMessage
                                                )
import           XMonad.StackSet                ( Workspace(Workspace) )
import           Control.Lens                   ( Lens
                                                , Lens'
                                                , (^.)
                                                , (%~)
                                                , (.~)
                                                , lens
                                                , (&)
                                                , _2
                                                , _Just
                                                )
import XMonad.Lens

--- Message Types

data MsgToggleFS = MsgToggleFS

instance Message MsgToggleFS

data MsgToggleGaps = MsgToggleGaps

instance Message MsgToggleGaps

--- FS state type

data FSState = FullScreen | NotFullScreen
  deriving (Show, Read)

toggleFS :: FSState -> FSState
toggleFS FullScreen    = NotFullScreen
toggleFS NotFullScreen = FullScreen

--- Gaps state type

data GapsState = Gaps | NoGaps
  deriving (Show, Read)

toggleGaps :: GapsState -> GapsState
toggleGaps Gaps = NoGaps
toggleGaps NoGaps = Gaps

data ToggleLayout l a = ToggleLayout {
    fsState :: FSState ,
    gapsState :: GapsState,
    innerLayout :: l a
}
  deriving (Show, Read)

lFSState :: Lens' (ToggleLayout l a) FSState
lFSState = lens fsState (\t n -> t { fsState = n })

lGapsState :: Lens' (ToggleLayout l a) GapsState
lGapsState = lens gapsState (\t n -> t { gapsState = n })

lInnerLayout :: Lens' (ToggleLayout l a) (l a)
lInnerLayout = lens innerLayout (\t n -> t { innerLayout = n })

runLayoutTL
    :: forall l a
     . LayoutClass l a
    => Workspace WorkspaceId (ToggleLayout l a) a
    -> Rectangle
    -> X ([(a, Rectangle)], Maybe (ToggleLayout l a))
runLayoutTL ws rect = do
    let layout = ws ^. lWSLayout
    let gapWidth = 
            case layout ^. lGapsState of
                Gaps -> 6
                NoGaps -> 0
    case layout ^. lFSState of
        FullScreen -> do
            result <- runLayout (ws & lWSLayout .~ Full) rect
            return $ result & _2 . _Just .~ layout
        NotFullScreen -> do
            let innerLayout = (ws & lWSLayout .~ layout ^. lInnerLayout)
            result <- runLayout innerLayout rect
            return $ result & _2 . _Just %~ \newLayout -> layout & lInnerLayout .~ newLayout

handleMessageTL
    :: LayoutClass l a
    => ToggleLayout l a
    -> SomeMessage
    -> X (Maybe (ToggleLayout l a))
handleMessageTL l msg | Just MsgToggleFS <- fromMessage msg =
    return $ Just $ l & lFSState %~ toggleFS
handleMessageTL l msg | Just MsgToggleGaps <- fromMessage msg =
    return $ Just $ l & lGapsState %~ toggleGaps
handleMessageTL l msg = do
    maybeLayout <- handleMessage (l ^. lInnerLayout) msg
    return $ fmap (\inner -> l & lInnerLayout .~ inner) maybeLayout

instance (Show a, LayoutClass l a) => LayoutClass (ToggleLayout l) a where
    runLayout = runLayoutTL
    handleMessage = handleMessageTL

addToggles :: l a -> ToggleLayout l a
addToggles = ToggleLayout NotFullScreen NoGaps 
