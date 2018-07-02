module DisplayConstants
    where
      
import Prelude
import Disk (Color(..))

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Must leave space at end, for sake of concat
---------------------------------------------------------------------
---------------------------------------------------------------------

basicGridItem :: String
basicGridItem =
    " board-grid-item h3 w3 "


fillableGridItem :: String
fillableGridItem =
    basicGridItem <> " flex justify-center "


defaultSquareColor :: String
defaultSquareColor = 
    " bg-light-gray "


moveSquareColor :: String
moveSquareColor = 
    " bg-green " 


moveSquareColor_FocusedMoveSquare :: String
moveSquareColor_FocusedMoveSquare =
    " bg-dark-green " 


moveSquareColor_FocusedFilledOpponentSquare :: String
moveSquareColor_FocusedFilledOpponentSquare = 
    " bg-dark-green " 


outflankSquareColor_FocusedMoveSquare :: String
outflankSquareColor_FocusedMoveSquare = 
    " bg-light-green "


outflankSquareColor_FocusedFilledOpponentSquare :: String
outflankSquareColor_FocusedFilledOpponentSquare = 
    " bg-light-yellow "


squareBorder_Default :: String
squareBorder_Default = 
    " outline " 


moveSquareBorder_FocusedMoveSquare :: String
moveSquareBorder_FocusedMoveSquare = 
    " outline " 


moveSquareBorder_FocusedFilledOpponentSquare :: String
moveSquareBorder_FocusedFilledOpponentSquare = 
    " outline " 


outflankSquareBorder_FocusedMoveSquare :: String
outflankSquareBorder_FocusedMoveSquare = 
    " outline "  -- " ba bw1 b--dotted " 


outflankSquareBorder_FocusedFilledOpponentSquare :: String
outflankSquareBorder_FocusedFilledOpponentSquare = 
    " outline "  -- " ba bw1 b--dotted " 


basicDisk :: String
basicDisk =
    " br-100 h2 w2 self-center ba " 


potentialDisk :: String
potentialDisk =
    basicDisk <> " bw1 b--orange " 


flipDisk :: String
flipDisk =
    basicDisk <> " bw1 b--orange b--dashed " 


placedDisk :: String
placedDisk =
    basicDisk


unusedDisk :: String
unusedDisk =
    " br-100 h1 w1 self-center ba " 


potentialDisk_Black :: String
potentialDisk_Black = 
    potentialDisk <> " bg-black "
        

potentialDisk_White :: String
potentialDisk_White = 
    potentialDisk <> " bg-white "          


flipDisk_Black :: String
flipDisk_Black = 
    flipDisk <> " bg-black "
        

flipDisk_White :: String
flipDisk_White = 
    flipDisk <> " bg-white "  


placedDisk_Black :: String
placedDisk_Black = 
    placedDisk <> " bg-black "
        

placedDisk_White :: String
placedDisk_White = 
    placedDisk <> " bg-white "        


unusedDisk_Black :: String
unusedDisk_Black = 
    unusedDisk <> " bg-black "    


unusedDisk_White :: String
unusedDisk_White = 
    unusedDisk <> " bg-white " 


potentialDiskClassesForColor :: Color -> String
potentialDiskClassesForColor color =
    case color of
        Black -> potentialDisk_Black 
        White -> potentialDisk_White


flipDiskClassesForColor :: Color -> String
flipDiskClassesForColor color =
    case color of
        Black -> flipDisk_Black  
        White -> flipDisk_White


placedDiskClassesForColor :: Color -> String
placedDiskClassesForColor color =
    case color of
        Black -> placedDisk_Black
        White -> placedDisk_White        


unusedDiskClassesForColor :: Color -> String
unusedDiskClassesForColor color =
    case color of
        Black -> unusedDisk_Black
        White -> unusedDisk_White           