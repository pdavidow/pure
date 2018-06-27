module DisplayConstants
    where
      
import Prelude


---------------------------------------------------------------------
---------------------------------------------------------------------
-- Must leave space at end, for sake of concat
---------------------------------------------------------------------
---------------------------------------------------------------------


defaultSquareColor :: String
defaultSquareColor = 
    " bg-light-gray "


moveSquareColor :: String
moveSquareColor = 
    " bg-light-green " 


moveSquareColor_InFocusMoveSquare :: String
moveSquareColor_InFocusMoveSquare =
    " bg-green " 


moveSquareColor_InFocusFilledOpponentSquare :: String
moveSquareColor_InFocusFilledOpponentSquare = 
    " bg-green " 


outflankSquareColor_InFocusMoveSquare :: String
outflankSquareColor_InFocusMoveSquare = 
    " bg-washed-green "


outflankSquareColor_InFocusFilledOpponentSquare :: String
outflankSquareColor_InFocusFilledOpponentSquare = 
    " bg-washed-yellow "


squareBorder_Default :: String
squareBorder_Default = 
    " outline " 


moveSquareBorder_InFocusMoveSquare :: String
moveSquareBorder_InFocusMoveSquare = 
    " outline " 


moveSquareBorder_InFocusFilledOpponentSquare :: String
moveSquareBorder_InFocusFilledOpponentSquare = 
    " outline " 


outflankSquareBorder_InFocusMoveSquare :: String
outflankSquareBorder_InFocusMoveSquare = 
    " outline "  -- " ba bw1 b--dotted " 


outflankSquareBorder_InFocusFilledOpponentSquare :: String
outflankSquareBorder_InFocusFilledOpponentSquare = 
    " outline "  -- " ba bw1 b--dotted " 


potentialDisk :: String
potentialDisk =
    " br-100 h-75 w-75 self-center ba " 


placedDisk :: String
placedDisk =
    " br-100 h2 w2 self-center ba " 


potentialDisk_Black :: String
potentialDisk_Black = 
    potentialDisk <> " bg-black "
        

potentialDisk_White :: String
potentialDisk_White = 
    potentialDisk <> " bg-white "          


placedDisk_Black :: String
placedDisk_Black = 
    placedDisk <> " bg-black "
        

placedDisk_White :: String
placedDisk_White = 
    placedDisk <> " bg-white "        