module DisplayConstants
    ( defaultSquareColor
    , moveSquareColor
    , outflankSquareColor
    , defaultSquareBorder
    , outflankSquareBorder
    , blackDisk
    , whiteDisk
    )
    where
      

import Prelude

-- Must leave space at end, for sake of concat

defaultSquareColor :: String
defaultSquareColor = 
    " bg-light-gray "


moveSquareColor :: String
moveSquareColor = 
    " bg-light-green " 


outflankSquareColor :: String
outflankSquareColor = 
    " bg-washed-green "


defaultSquareBorder :: String
defaultSquareBorder = 
    " outline " 


outflankSquareBorder :: String
outflankSquareBorder = 
    " ba bw1 b--dotted " 


disk :: String
disk =
    " br-100 h2 w2 self-center ba " 


blackDisk :: String
blackDisk = 
    disk <> " bg-black "
        

whiteDisk :: String
whiteDisk = 
    disk <> " bg-white "        