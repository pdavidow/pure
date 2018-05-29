module Position 
    ( PositionRec
    , PositionRow(..)
    )
    where

import Prelude

import Data.Record (equal)
import Data.Foldable (and)
import Data.List (List, length, zipWith)

type PositionRec = {x :: Int, y :: Int} -- one-based

newtype PositionRow = PositionRow (List PositionRec)

instance eqPositionRow :: Eq PositionRow where
    eq (PositionRow list1) (PositionRow list2) = 
        -- https://functionalprogramming.slack.com/archives/C717K38CE/p1527536609000069
        (length list1 == length list2) && (and $ zipWith equal list1 list2)
  