module BlackWhite
    ( BlackWhite(..)
    , BlackWhiteH(..)
    , makeBlackWhite
    , makeBlackWhiteH
    )
    where


data BlackWhite a = BlackWhite {black :: a, white :: a}
data BlackWhiteH a b = BlackWhiteH {black :: a, white :: b}


makeBlackWhite :: forall a. a -> a -> BlackWhite a
makeBlackWhite b w =
    BlackWhite {black: b, white: w}


makeBlackWhiteH :: forall a b. a -> b -> BlackWhiteH a b
makeBlackWhiteH b w =
    BlackWhiteH {black: b, white: w}    
