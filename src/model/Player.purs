module Player
    ( Player(..)
    , PlayerType(..)
    , Players 
    , playerColored
    , setCurrentPlayerColorForSearch
    , isPlayer_Person
    , isPlayer_Computer
    , isComputerVsComputer
    , isPersonVsPerson
    , isPersonVsComputer
    , blackPlayer
    , whitePlayer
    )
    where
 
import Prelude

import BlackWhite (BlackWhite, getItemColored, getItemBlack, getItemWhite)
import Disk (Color)
import GameState (Core(..), Tagged_GameState, core_FromTaggedGameState, swapCore)
import Search (SearchDepth)
import Type.Data.Boolean (kind Boolean)


data Player = Player Color PlayerType

data PlayerType
    = Computer 
        { searchDepth :: SearchDepth
        , isRandomPick :: Boolean
        } 
    | Person 
        { searchDepth :: SearchDepth
        , isAutoSuggest :: Boolean
        }   
       
type Players = BlackWhite Player


derive instance eqPlayerType :: Eq PlayerType    
derive instance eqPlayer :: Eq Player       


playerColored :: Players -> Color -> Player
playerColored players color =
    getItemColored color players


blackPlayer :: Players -> Player
blackPlayer players =
    getItemBlack players


whitePlayer :: Players -> Player
whitePlayer players =
    getItemWhite players


isComputerVsComputer :: Players -> Boolean
isComputerVsComputer players =
    (isPlayer_Computer $ blackPlayer players) && (isPlayer_Computer $ whitePlayer players)


isPersonVsPerson :: Players -> Boolean
isPersonVsPerson players =
    (isPlayer_Person $ blackPlayer players) && (isPlayer_Person $ whitePlayer players)


isPersonVsComputer :: Players -> Boolean
isPersonVsComputer players =
    ( (isPlayer_Person $ blackPlayer players) && (isPlayer_Computer $ whitePlayer players) )
    ||
    ( (isPlayer_Computer $ blackPlayer players) && (isPlayer_Person $ whitePlayer players) )


-- todo use State monad for search...?
setCurrentPlayerColorForSearch :: Tagged_GameState -> Color -> Tagged_GameState
setCurrentPlayerColorForSearch taggedGameState color = 
    let
        (Core rec) = core_FromTaggedGameState taggedGameState
        core' = Core $ rec {currentPlayerColorForSearch = color}
    in
        swapCore taggedGameState core'


isPlayer_Person :: Player -> Boolean                 
isPlayer_Person (Player _ playerType) =
    case playerType of
        Person   _ -> true
        Computer _ -> false    


isPlayer_Computer :: Player -> Boolean                 
isPlayer_Computer (Player _ playerType) =
    case playerType of
        Person   _ -> false
        Computer _ -> true