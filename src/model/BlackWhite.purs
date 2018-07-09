module BlackWhite
    ( BlackWhite(..)
    , makeBlackWhite
    , ofColor -- todo use more
    , black 
    , white 
    )
    where

import Prelude
import Disk (Color(..))

data BlackWhite a = BlackWhite {black :: a, white :: a}


derive instance eqBlackWhite :: Eq a => Eq (BlackWhite a)


instance showBlackWhite :: Show a => Show (BlackWhite a) where
    show (BlackWhite {black: b, white: w}) = 
        "Black: " <> show b <> ", White: " <> show w


makeBlackWhite :: forall a. a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite {black: b, white: w}


ofColor :: forall a. Color -> BlackWhite a -> a
ofColor color (BlackWhite rec) =
    case color of
        Black -> rec.black
        White -> rec.white


black :: forall a. BlackWhite a -> a
black (BlackWhite rec) = 
    rec.black


white :: forall a. BlackWhite a -> a
white (BlackWhite rec) = 
    rec.white    