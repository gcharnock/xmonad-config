
module XMonad.Lens where

import           XMonad
import           XMonad.StackSet                ( Workspace(Workspace), Stack(..) )
import           Control.Lens                   ( Lens
                                                , Lens'
                                                , lens
                                                )

lWSLayout :: Lens (Workspace id l1 a) (Workspace id l2 a) l1 l2
lWSLayout = lens get set
  where
    get (Workspace _ l _) = l
    set (Workspace ident _ a) l = Workspace ident l a

lWSStack :: Lens' (Workspace id l a) (Maybe (Stack a))
lWSStack = lens get set
  where
    get (Workspace _ _ s) = s
    set (Workspace ident l _) s = Workspace ident l s

lRectHeight :: Lens' Rectangle Dimension
lRectHeight = lens get set
  where
    get (Rectangle {rect_height}) = rect_height
    set (Rectangle {rect_x, rect_y, rect_width}) rect_height = Rectangle
        { rect_height
        , rect_width
        , rect_x
        , rect_y
        }

lRectWidth :: Lens' Rectangle Dimension
lRectWidth = lens get set
  where
    get (Rectangle {rect_width}) = rect_width
    set (Rectangle {rect_x, rect_y, rect_height}) rect_width = Rectangle
        { rect_height
        , rect_width
        , rect_x
        , rect_y
        }

lRectX :: Lens' Rectangle Position 
lRectX = lens get set
  where
    get (Rectangle {rect_x}) = rect_x
    set (Rectangle {rect_y, rect_height, rect_width}) rect_x = Rectangle
        { rect_height
        , rect_width
        , rect_x
        , rect_y
        }

lRectY :: Lens' Rectangle Position
lRectY = lens get set
  where
    get (Rectangle {rect_y}) = rect_y 
    set (Rectangle {rect_x, rect_height, rect_width}) rect_y = Rectangle
        { rect_height
        , rect_width
        , rect_x
        , rect_y
        }