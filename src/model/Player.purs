module Player
    ( Player(..)
    , PlayerType(..)
    , Players 
    --, moveSequence
    , mbSuggestedMove
    , isPlayer_Person
    , isPlayer_Computer
    , isComputerVsComputer
    )
    where
 
import Prelude

import BlackWhite (BlackWhite, getItemColored, getItemBlack, getItemWhite)
import Board (Move)
import Data.Maybe (Maybe(..))
import Disk (Color)
import GameState (Core(..), Tagged_GameState(..), mbNextMoveColor_FromTaggedGameState, core_FromTaggedGameState, swapCore)
import Search (Strategy, SearchDepth, mbBestNextMove)
import Type.Data.Boolean (kind Boolean)


newtype Player = Player PlayerType

data PlayerType
    = Person 
        { suggestionSearchDepth :: SearchDepth
        , isAutoSuggest :: Boolean
        }
    | Computer Strategy
       
type Players = BlackWhite Player


derive instance eqPlayerType :: Eq PlayerType    
derive instance eqPlayer :: Eq Player       


isComputerVsComputer :: Players -> Boolean
isComputerVsComputer players =
    (isPlayer_Computer $ getItemBlack players) && (isPlayer_Computer $ getItemWhite players)
     

-- moveSequence :: Players -> GameHistory -> GameHistory
-- moveSequence players gameHistory =
--     let
--         taggedGameState = NE.last history
--     in



mbSuggestedMove :: Players -> Tagged_GameState -> Maybe Move         
mbSuggestedMove players taggedGameState =
    case mbNextMoveColor_FromTaggedGameState taggedGameState of
        Nothing -> Nothing
        Just color -> mbSuggestedMoveForColor players taggedGameState color


mbSuggestedMoveForColor :: Players -> Tagged_GameState -> Color -> Maybe Move
mbSuggestedMoveForColor players taggedGameState color =
    let
        (Player playerType) = getItemColored color players        
    in
        case playerType of
            Person rec ->
                if rec.isAutoSuggest then
                    let
                        taggedGameState' = setCurrentPlayerColorOn taggedGameState color
                    in
                        mbBestNextMove rec.suggestionSearchDepth taggedGameState'
                else
                    Nothing
            Computer _ ->
                Nothing    


setCurrentPlayerColorOn :: Tagged_GameState -> Color -> Tagged_GameState
setCurrentPlayerColorOn taggedGameState color = 
    let
        (Core rec) = core_FromTaggedGameState taggedGameState
        core' = Core $ rec {currentPlayerColorForSearch = color}
    in
        swapCore taggedGameState core'


isPlayer_Person :: Player -> Boolean                 
isPlayer_Person (Player playerType) =
    case playerType of
        Person   _ -> true
        Computer _ -> false    


isPlayer_Computer :: Player -> Boolean                 
isPlayer_Computer (Player playerType) =
    case playerType of
        Person   _ -> false
        Computer _ -> true       
