module Lib 
    ( haskellRange
    , mapTakeWhile
    , setCssProp
    )
    where
  
import Prelude
import Data.Array (range)
import Data.List (List(..), (:))
import Halogen as H
import Halogen.HTML.CSS as HC
import CSS as CSS

 
-- todo https://pursuit.purescript.org/packages/purescript-enums/3.2.1/docs/Data.Enum#v:enumFromTo
-- https://purescript-users.ml/t/ranges-in-haskell-and-purescript-differ/205
haskellRange :: Int -> Int -> Array Int
haskellRange start end =
    if start > end then
        []
    else
        range start end


mapTakeWhile :: forall a b. (a -> b) -> (b -> Boolean) -> List a -> List b
mapTakeWhile _ _ Nil = Nil
mapTakeWhile f p (x:xs) = let y = f x in if p y then y : mapTakeWhile f p xs else Nil    


setCssProp :: String -> String -> forall t1 t2. H.IProp ( style :: String | t1) t2
setCssProp key value =
    HC.style do (CSS.key (CSS.fromString key) value)