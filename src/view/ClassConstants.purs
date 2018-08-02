module ClassConstants
    where
      
import Prelude

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Must leave space at end of string, for sake of concat (<>)
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


winEndedSquareColor :: String
winEndedSquareColor =
    " bg-moon-gray "


tieEndedSquareColor :: String
tieEndedSquareColor =
    " bg-ightest-blue "


squareBorder_Default :: String
squareBorder_Default = 
    " outline " 


moveSquareBorder_NonSuggested :: String
moveSquareBorder_NonSuggested = 
    squareBorder_Default 


moveSquareBorder_Suggested :: String
moveSquareBorder_Suggested = 
    " ba bw2 b--light-purple " 


filledSquareBorder_PriorMove :: String
filledSquareBorder_PriorMove = 
    " ba bw2 b--black "         


filledSquareBorder_OutflankOfPriorMove :: String
filledSquareBorder_OutflankOfPriorMove =
    " ba bw2 b--black b--dotted "   


outflankSquareBorder_FocusedMoveSquare :: String
outflankSquareBorder_FocusedMoveSquare = 
    squareBorder_Default  -- " ba bw1 b--dotted " 


outflankSquareBorder_FocusedFilledOpponentSquare :: String
outflankSquareBorder_FocusedFilledOpponentSquare = 
    squareBorder_Default  -- " ba bw1 b--dotted " 


basicDisk :: String
basicDisk =
    " br-100 h2 w2 self-center flex justify-center ba "


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


flipCountText :: String
flipCountText =
    " self-center gray f4 b roboto noselect "             


gameOver_Emphasis ::  String
gameOver_Emphasis =       
    " b "
      

-- todo unused
modalSettingsId :: String
modalSettingsId =
    " modal-ter "      


isActive :: String
isActive =
    " is-active "      


isInvisible :: String
isInvisible =
    " is-invisible "  -- https://bulma.io/documentation/modifiers/helpers/          