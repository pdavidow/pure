module Lib 
    ( haskellRange
    )
    where
  
import Prelude
import Data.Array (range)


-- https://functionalprogramming.slack.com/archives/C717K38CE/p1527746985000170
haskellRange :: Int -> Int -> Array Int
haskellRange start end =
    if start > end then
        []
    else
        range start end