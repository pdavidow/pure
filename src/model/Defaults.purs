module Defaults
    where

import Prelude
import BlackWhite (makeBlackWhite)
import Player (Player(..), PlayerType(..), Players)
import Search (SearchDepth(..), Strategy(..))


defaultPlayers :: Players
defaultPlayers =
    makeBlackWhite playerBlack playerWhite
    where
        playerBlack = Player defaultPlayerType_Person              
        playerWhite = Player defaultPlayerType_Computer 
    

defaultSearchDepth :: SearchDepth
defaultSearchDepth =
    SearchDepth_3


defaultPlayerType_Computer :: PlayerType
defaultPlayerType_Computer =
    Computer $ SearchDepth defaultSearchDepth


defaultPlayerType_Person :: PlayerType
defaultPlayerType_Person =
    Person  
        { suggestionSearchDepth: defaultSearchDepth
        , isAutoSuggest: false 
        } 