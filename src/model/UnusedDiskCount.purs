module UnusedDiskCount
    ( BlackUnusedDiskCount -- hiding constructor
    , WhiteUnusedDiskCount -- hiding constructor
    , UnusedDiskCounts
    , Tagged_UnusedDiskCount(..)
    , makeUnusedDiskCounts
    , isZeroCount
    , transferDiskTo
    , decreaseByOneFor
    , countFrom
    , maxDiskCount
    )
    where 

import Prelude
import BlackWhite (BlackWhiteH(..), makeBlackWhiteH)
import BoardSize (boardSize)
import Disk (Color(..))


newtype BlackUnusedDiskCount = BlackUnusedDiskCount Int

newtype WhiteUnusedDiskCount = WhiteUnusedDiskCount Int

type UnusedDiskCounts = BlackWhiteH BlackUnusedDiskCount WhiteUnusedDiskCount

data Tagged_UnusedDiskCount
    = Tagged_BlackUnusedDiskCount BlackUnusedDiskCount
    | Tagged_WhiteUnusedDiskCount WhiteUnusedDiskCount
 
 
derive instance eqBlackUnusedDiskCount :: Eq BlackUnusedDiskCount
derive instance eqWhiteUnusedDiskCount :: Eq WhiteUnusedDiskCount
derive instance eqTagged_UnusedDiskCount :: Eq Tagged_UnusedDiskCount


makeUnusedDiskCounts :: UnusedDiskCounts
makeUnusedDiskCounts =
    makeBlackWhiteH 
        (BlackUnusedDiskCount maxDiskCount) 
        (WhiteUnusedDiskCount maxDiskCount)


maxDiskCount :: Int
maxDiskCount =
    div (boardSize * boardSize) 2 -- apparently 


isZeroCount :: Tagged_UnusedDiskCount -> Boolean
isZeroCount tagged =
    let
        f :: Int -> Boolean
        f = \ n -> n == 0
    in
        case tagged of
            Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> f x
            Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> f x    


subtractOneForPositive :: Int -> Int
subtractOneForPositive n =
    if n < 1 then n else n - 1


transferDiskTo :: Color -> UnusedDiskCounts -> UnusedDiskCounts
transferDiskTo color (BlackWhiteH {black: (BlackUnusedDiskCount b), white: (WhiteUnusedDiskCount w)}) =
    case color of
        Black -> makeBlackWhiteH (BlackUnusedDiskCount $ b + 1) (WhiteUnusedDiskCount $ subtractOneForPositive w)
        White -> makeBlackWhiteH (BlackUnusedDiskCount $ subtractOneForPositive b) (WhiteUnusedDiskCount $ w + 1)


decreaseByOneFor :: Color -> UnusedDiskCounts -> UnusedDiskCounts
decreaseByOneFor color (BlackWhiteH {black: blackUnusedDiskCount@(BlackUnusedDiskCount b), white: whiteUnusedDiskCount@(WhiteUnusedDiskCount w)}) =
    case color of
        Black -> makeBlackWhiteH (BlackUnusedDiskCount $ subtractOneForPositive b) whiteUnusedDiskCount
        White -> makeBlackWhiteH blackUnusedDiskCount (WhiteUnusedDiskCount $ subtractOneForPositive w)
        

countFrom :: Tagged_UnusedDiskCount -> Int
countFrom tagged =
    case tagged of
        Tagged_BlackUnusedDiskCount (BlackUnusedDiskCount x) -> x
        Tagged_WhiteUnusedDiskCount (WhiteUnusedDiskCount x) -> x                