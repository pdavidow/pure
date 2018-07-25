module Lib 
    ( haskellRange
    , mapTakeWhile
    , rights
    , lefts
    )
    where
  
import Prelude
import Data.Array (range)
import Data.List (List(..), (:), filter)
import Data.Either (Either, fromRight, fromLeft, isRight, isLeft)
import Partial.Unsafe (unsafePartial)


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


rights :: forall l r. List (Either l r) -> List r
rights xs =
    xs
        # filter isRight    
        # map (unsafePartial fromRight)


lefts :: forall l r. List (Either l r) -> List l
lefts xs =
    xs
        # filter isLeft    
        # map (unsafePartial fromLeft)        
