module Disk
    ( Disk -- hiding constructor
    , Color(..)
    , diskColor
    , flipCount
    , flipDisk
    , makeDisk
    , toggleColor
    )
    where

import Prelude
import Data.Int (even)


newtype Disk = Disk {initColor :: Color, flipCount :: Int}

-- todo replace with...?
-- https://pursuit.purescript.org/packages/purescript-colors/4.3.0/docs/Color#v:black
-- https://pursuit.purescript.org/packages/purescript-colors/4.3.0/docs/Color#v:white
data Color = Black | White


derive instance eqDisk :: Eq Disk
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color

instance showDisk :: Show Disk where
    show (Disk rec) = 
        "Disk {initColor: " <> show rec.initColor <> ", flipCount: " <> show rec.flipCount <> "}"


instance showColor :: Show Color where
    show color = 
        case color of
            Black -> "Black"
            White -> "White"


makeDisk :: Color -> Disk
makeDisk color = 
    Disk {initColor: color, flipCount: 0}


diskColor :: Disk -> Color
diskColor (Disk rec) =
    if even rec.flipCount then 
        rec.initColor
    else 
        toggleColor rec.initColor


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White        
     

flipDisk :: Disk -> Disk
flipDisk (Disk ({initColor: color, flipCount: count})) =
    Disk {initColor: color, flipCount: count + 1}    


flipCount :: Disk -> Int
flipCount (Disk rec) =
    rec.flipCount