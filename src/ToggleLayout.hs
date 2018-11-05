
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

data MsgToggleFS = MsgToggleFS

instance Message MsgToggleFS

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

instance forall l a. (Show a, LayoutClass l a) => LayoutClass (ToggleLayout l) a where
    runLayout ws rect = do
            let layout = ws ^. lWSLayout
            case layout ^. lFSState of
                 FullScreen -> do
                   (windows, _) <- runLayout (ws & lWSLayout .~ Full) rect
                   return $ (windows, Nothing)
                 NotFullScreen -> do
                   result <- runLayout (ws & lWSLayout .~ layout ^. linnerLayout) rect
                   return $ result & _2 %~ fmap ((\l -> layout & linnerLayout .~ l) :: l a -> ToggleLayout l a)

    handleMessage (ToggleLayout fsState gapsState l) msg = do
        let layout' = lFSState %~ toggleFS
        let maybeSwap = fromMessage msg
        case maybeSwap of
            Nothing -> do
                maybeLayout <- handleMessage l msg
                return $ fmap (\l -> ToggleLayout fsState gapsState l) maybeLayout
            Just MsgToggleFS ->
                case fsState of
                    FullScreen ->
                        return $ Just $ ToggleLayout NotFullScreen gapsState l
                    NotFullScreen ->
                        return $ Just $ ToggleLayout FullScreen gapsState l

addToggles :: l a -> ToggleLayout l a
addToggles = ToggleLayout NotFullScreen Gaps
