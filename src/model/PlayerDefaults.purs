module PlayerDefaults
    where


import BlackWhite (makeBlackWhite)
import Disk (Color(..))
import Player (Player(..), PlayerType(..), Players)
import Search (SearchDepth(..))
import Type.Data.Boolean (kind Boolean)


defaultPlayers :: Players
defaultPlayers =
    makeBlackWhite 
        (Player Black defaultPerson_PlayerType)              
        (Player White defaultComputer_PlayerType)


defaultComputer_PlayerType :: PlayerType
defaultComputer_PlayerType =
    Computer
        { searchDepth: defaultComputer_searchDepth
        , isRandomPick: defaultComputer_isRandomPick
        } 


defaultPerson_PlayerType :: PlayerType
defaultPerson_PlayerType =
    Person  
        { searchDepth: defaultPerson_searchDepth
        , isAutoSuggest: defaultPerson_isAutoSuggest 
        } 


defaultComputer_isRandomPick :: Boolean
defaultComputer_isRandomPick = 
    false


defaultComputer_searchDepth :: SearchDepth
defaultComputer_searchDepth =
    SearchDepth_2


defaultPerson_searchDepth :: SearchDepth
defaultPerson_searchDepth =
    SearchDepth_3


defaultPerson_isAutoSuggest :: Boolean
defaultPerson_isAutoSuggest =
    false        