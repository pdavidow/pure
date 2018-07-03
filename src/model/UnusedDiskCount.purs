module UnusedDiskCount
    ( UnusedDiskCounts
    , makeUnusedDiskCounts
    , transferDiskTo
    , decreaseByOneFor
    , maxDiskCount
    , black
    , white
    )
    where 

import Prelude
import BlackWhite (BlackWhite(..), makeBlackWhite)
import BoardSize (boardSize)
import Disk (Color(..))

type UnusedDiskCounts = BlackWhite Int


makeUnusedDiskCounts :: UnusedDiskCounts
makeUnusedDiskCounts =
    makeBlackWhite maxDiskCount maxDiskCount


maxDiskCount :: Int
maxDiskCount =
    div (boardSize * boardSize) 2 -- apparently 


subtractOneForPositive :: Int -> Int
subtractOneForPositive n =
    if n < 1 then n else n - 1


transferDiskTo :: Color -> UnusedDiskCounts -> UnusedDiskCounts
transferDiskTo color (BlackWhite {black: b, white: w}) =
    case color of
        Black -> makeBlackWhite (b + 1) (subtractOneForPositive w)
        White -> makeBlackWhite (subtractOneForPositive b) (w + 1)


decreaseByOneFor :: Color -> UnusedDiskCounts -> UnusedDiskCounts
decreaseByOneFor color (BlackWhite {black: b, white: w}) =
    case color of
        Black -> makeBlackWhite (subtractOneForPositive b) w
        White -> makeBlackWhite b (subtractOneForPositive w)


black :: UnusedDiskCounts -> Int
black (BlackWhite {black: x, white: _}) =
    x


white :: UnusedDiskCounts -> Int
white (BlackWhite {black: _, white: x}) =
    x     