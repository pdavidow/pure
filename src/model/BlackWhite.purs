module BlackWhite
    ( BlackWhite(..)
    , makeBlackWhite
    , getItemColored -- todo use more
    , getItemBlack 
    , getItemWhite 
    , setItemColored 
    , setItemBlack 
    , setItemWhite     
    )
    where

import Prelude
import Disk (Color(..))

newtype BlackWhite a = BlackWhite {black :: a, white :: a}


derive instance eqBlackWhite :: Eq a => Eq (BlackWhite a)


instance showBlackWhite :: Show a => Show (BlackWhite a) where
    show (BlackWhite {black: b, white: w}) = 
        "Black: " <> show b <> ", White: " <> show w


makeBlackWhite :: forall a. a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite {black: b, white: w}


getItemColored :: forall a. Color -> BlackWhite a -> a
getItemColored color x =
    case color of
        Black -> getItemBlack x
        White -> getItemWhite x


getItemBlack :: forall a. BlackWhite a -> a
getItemBlack (BlackWhite rec) = 
    rec.black


getItemWhite :: forall a. BlackWhite a -> a
getItemWhite (BlackWhite rec) = 
    rec.white    


setItemColored :: forall a. Color -> BlackWhite a -> a -> BlackWhite a
setItemColored color bw elem =
    case color of
        Black -> setItemBlack bw elem
        White -> setItemWhite bw elem


setItemBlack :: forall a. BlackWhite a -> a -> BlackWhite a
setItemBlack (BlackWhite rec) x =       
    BlackWhite $ rec {black = x}


setItemWhite :: forall a. BlackWhite a -> a -> BlackWhite a
setItemWhite (BlackWhite rec) x =       
    BlackWhite $ rec {white = x}    