module BlackWhite
    ( BlackWhite(..)
    , makeBlackWhite
    )
    where


data BlackWhite a = BlackWhite {black :: a, white :: a}


makeBlackWhite :: forall a. a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite {black: b, white: w}
