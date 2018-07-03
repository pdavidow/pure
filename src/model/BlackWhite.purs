module BlackWhite
    ( BlackWhite(..)
    , makeBlackWhite
    )
    where

import Prelude

data BlackWhite a = BlackWhite {black :: a, white :: a}


derive instance eqBlackWhite :: Eq a => Eq (BlackWhite a)


makeBlackWhite :: forall a. a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite {black: b, white: w}