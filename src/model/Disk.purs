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


data Disk = Disk {initColor :: Color, flipCount :: Int}

data Color = Black | White


derive instance eqDisk :: Eq Disk
derive instance eqColor :: Eq Color
derive instance ordColor :: Ord Color

instance showDisk :: Show Disk where
    show (Disk ({initColor: color, flipCount: count})) = 
        "Disk {initColor: " <> show color <> ", flipCount: " <> show count <> "}"


instance showColor :: Show Color where
    show color = 
        case color of
            Black -> "Black"
            White -> "White"


makeDisk :: Color -> Disk
makeDisk color = 
    Disk {initColor: color, flipCount: 0}


diskColor :: Disk -> Color
diskColor (Disk ({initColor: color, flipCount: count})) =
    if even count then 
        color
    else 
        toggleColor color


toggleColor :: Color -> Color
toggleColor color = 
    case color of
        White -> Black
        Black -> White        
     

flipDisk :: Disk -> Disk
flipDisk (Disk ({initColor: color, flipCount: count})) =
    Disk {initColor: color, flipCount: count + 1}    


flipCount :: Disk -> Int
flipCount (Disk ({initColor: color, flipCount: count})) =
    count