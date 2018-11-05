
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonadConfig where

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
                                                )
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

data MsgToggleFS = MsgToggleFS

instance Message MsgToggleFS

data MsgToggleGaps = MsgToggleGaps

instance Message MsgToggleGaps

data FSState = FullScreen | NotFullScreen
  deriving (Show, Read)

toggleFS :: FSState -> FSState
toggleFS FullScreen    = NotFullScreen
toggleFS NotFullScreen = FullScreen

data GapsState = Gaps | NoGaps
  deriving (Show, Read)

lWSLayout :: Lens (Workspace id l1 a) (Workspace id l2 a) l1 l2
lWSLayout = lens get set
  where
    get (Workspace _ l _) = l
    set (Workspace id _ a) l = Workspace id l a

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

linnerLayout :: Lens' (ToggleLayout l a) (l a)
linnerLayout = lens innerLayout (\t n -> t { innerLayout = n })

runLayoutTL
    :: forall l a
     . LayoutClass l a
    => Workspace WorkspaceId (ToggleLayout l a) a
    -> Rectangle
    -> X ([(a, Rectangle)], Maybe (ToggleLayout l a))
runLayoutTL ws rect = do
    let layout = ws ^. lWSLayout
    case layout ^. lFSState of
        FullScreen -> do
            (windows, _) <- runLayout (ws & lWSLayout .~ Full) rect
            return $ (windows, Nothing)
        NotFullScreen -> do
            result <- runLayout (ws & lWSLayout .~ layout ^. linnerLayout) rect
            return
                $  result
                &  _2
                %~ fmap
                       ((\l -> layout & linnerLayout .~ l) :: l a
                         -> ToggleLayout l a
                       )


handleMessageTL :: LayoutClass l a=> ToggleLayout l a -> SomeMessage -> X (Maybe (ToggleLayout l a))
handleMessageTL (ToggleLayout fsState gapsState l) msg = do
    handled <- runMaybeT $ do
        let layout' = lFSState %~ toggleFS
        MsgToggleFS <- fromMessage msg
        case fsState of
            FullScreen ->
                return $ Just $ ToggleLayout NotFullScreen gapsState l
            NotFullScreen ->
                return $ Just $ ToggleLayout FullScreen gapsState l
    case handled of
        Nothing -> do
            maybeLayout <- handleMessage l msg
            return $ fmap (\l -> ToggleLayout fsState gapsState l) maybeLayout
        Just out -> return out



instance (Show a, LayoutClass l a) => LayoutClass (ToggleLayout l) a where
    runLayout = runLayoutTL
    handleMessage = handleMessageTL
    
addToggles :: l a -> ToggleLayout l a
addToggles = ToggleLayout NotFullScreen Gaps
