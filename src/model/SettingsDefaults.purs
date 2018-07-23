module SettingsDefaults
    where


import Prelude
import BlackWhite (makeBlackWhite)
import Disk (Color(..))
import Player (Player(..), PlayerType(..), Players)
import Search (SearchDepth(..), Strategy(..))


defaultPlayers :: Players
defaultPlayers =
    makeBlackWhite b w
    where
        b = Player Black defaultPlayerType_Computer               
        w = Player White defaultPlayerType_Computer
    

defaultSearchDepth :: SearchDepth
defaultSearchDepth =
    SearchDepth_3


defaultPlayerType_Computer :: PlayerType
defaultPlayerType_Computer =
    -- Computer $ SearchDepth SearchDepth_1
    Computer RandomPick


defaultPlayerType_Person :: PlayerType
defaultPlayerType_Person =
    Person  
        { suggestionSearchDepth: defaultSearchDepth
        , isAutoSuggest: true 
        } 