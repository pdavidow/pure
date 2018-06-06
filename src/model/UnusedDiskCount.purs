module UnusedDiskCount
    ( BlackUnusedDiskCount -- hiding constructor
    , WhiteUnusedDiskCount -- hiding constructor
    , UnusedDiskCounts
    , Tagged_UnusedDiskCount(..)
    , makeBlackUnusedDiskCount
    , makeWhiteUnusedDiskCount 
    , isZeroCount
    , transferDiskTo
    , decreaseByOneFor
    , countFrom
    , applyToUnusedDiskCounts
    )
    where

import Prelude
import BlackWhite (BlackWhiteH(..), makeBlackWhiteH)
import BoardSize (boardSize)
import Disk (Color(..))


newtype UnusedDiskCount = UnusedDiskCount Int

newtype BlackUnusedDiskCount = BlackUnusedDiskCount UnusedDiskCount

newtype WhiteUnusedDiskCount = WhiteUnusedDiskCount UnusedDiskCount

type UnusedDiskCounts = BlackWhiteH BlackUnusedDiskCount WhiteUnusedDiskCount

data Tagged_UnusedDiskCount
    = Tagged_BlackUnusedDiskCount BlackUnusedDiskCount
    | Tagged_WhiteUnusedDiskCount WhiteUnusedDiskCount


makeBlackUnusedDiskCount :: BlackUnusedDiskCount 
makeBlackUnusedDiskCount =
    BlackUnusedDiskCount initUnusedDiskCount


makeWhiteUnusedDiskCount :: WhiteUnusedDiskCount 
makeWhiteUnusedDiskCount =
    WhiteUnusedDiskCount initUnusedDiskCount


initUnusedDiskCount :: UnusedDiskCount
initUnusedDiskCount =
    UnusedDiskCount $ div (boardSize * boardSize) 2 -- apparently 


isZeroCount :: Tagged_UnusedDiskCount -> Boolean
isZeroCount tagged =
    let
        f :: UnusedDiskCount -> Boolean
        f = \ (UnusedDiskCount n) -> n == 0
    in
        case tagged of
            Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> f x
            Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> f x    

-- todo unused?
applyToCount :: (UnusedDiskCount -> UnusedDiskCount) -> Tagged_UnusedDiskCount -> Tagged_UnusedDiskCount
applyToCount f tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> Tagged_BlackUnusedDiskCount $ BlackUnusedDiskCount $ f x
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> Tagged_WhiteUnusedDiskCount $ WhiteUnusedDiskCount $ f x

-- todo remove? ... simply inline...
applyToUnusedDiskCounts :: (UnusedDiskCounts -> UnusedDiskCounts) -> UnusedDiskCounts -> UnusedDiskCounts
applyToUnusedDiskCounts f x =
    f x  


subtractOneForPositive :: Int -> Int
subtractOneForPositive n =
    if n < 1 then n else n - 1


transferDiskTo :: Color -> UnusedDiskCounts -> UnusedDiskCounts
transferDiskTo color (BlackWhiteH {black: (BlackUnusedDiskCount (UnusedDiskCount b)), white: (WhiteUnusedDiskCount (UnusedDiskCount w))}) =
    case color of
        Black -> makeBlackWhiteH (BlackUnusedDiskCount $ UnusedDiskCount $ b+1) (WhiteUnusedDiskCount $ UnusedDiskCount $ subtractOneForPositive w)
        White -> makeBlackWhiteH (BlackUnusedDiskCount $ UnusedDiskCount $ subtractOneForPositive b) (WhiteUnusedDiskCount $ UnusedDiskCount $ w+1)


decreaseByOneFor :: Color -> UnusedDiskCounts -> UnusedDiskCounts
decreaseByOneFor color (BlackWhiteH {black: blackUnusedDiskCount@(BlackUnusedDiskCount (UnusedDiskCount b)), white: whiteUnusedDiskCount@(WhiteUnusedDiskCount (UnusedDiskCount w))}) =
    case color of
        Black -> makeBlackWhiteH (BlackUnusedDiskCount $ UnusedDiskCount $ subtractOneForPositive b) whiteUnusedDiskCount
        White -> makeBlackWhiteH blackUnusedDiskCount (WhiteUnusedDiskCount $ UnusedDiskCount $ subtractOneForPositive w)
        

countFrom :: Tagged_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount (UnusedDiskCount n)) -> n
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount (UnusedDiskCount n)) -> n                  