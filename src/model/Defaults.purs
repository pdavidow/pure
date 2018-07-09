module Defaults
    where


import BlackWhite (makeBlackWhite)
import Disk (Color(..))
import Player (Player(..), PlayerType(..), Players)
import Search (SearchDepth(..))


defaultPlayers :: Players
defaultPlayers =
    makeBlackWhite playerBlack playerWhite
    where
        playerBlack =
            Player 
                { color: Black
                , type: Person { suggestionSearchDepth: defaultSearchDepth } 
                }
                
        playerWhite =
            Player 
                { color: White
                , type: Person { suggestionSearchDepth: defaultSearchDepth } 
                }  

    
defaultSearchDepth :: SearchDepth
defaultSearchDepth =
    SearchDepth_3