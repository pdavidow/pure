module Player
    ( Player(..)
    , PlayerType(..)
    , Players 
    , mbSuggestedMove
    )
    where
 
import Prelude
import Board (Move)
import Data.Maybe (Maybe(..))
import Disk (Color)
import GameState (Core(..), Tagged_GameState(..), nextMoveColor_FromStartGameState, core_FromTaggedGameState, nextMoveColor_FromMidGameState, swapCore) 
import Search (Strategy(..), SearchDepth, mbBestNextMove)
import BlackWhite (BlackWhite, ofColor) 

data Player = Player 
    { color :: Color
    , type :: PlayerType
    }

data PlayerType
    = Person {suggestionSearchDepth :: SearchDepth}
    | Computer Strategy
       
type Players = BlackWhite Player


derive instance eqPlayerType :: Eq PlayerType    
derive instance eqPlayer :: Eq Player       


mbSuggestedMove :: Players -> Tagged_GameState -> Maybe Move         
mbSuggestedMove players taggedGameState =
    let
        f :: Color -> Maybe Move
        f = \color -> searchOnPlayerColor players taggedGameState color
    in
        case taggedGameState of
            Tagged_StartGameState x -> f $ nextMoveColor_FromStartGameState x
            Tagged_MidGameState   x -> f $ nextMoveColor_FromMidGameState   x
            Tagged_EndedGameState _ -> Nothing


searchOnPlayerColor :: Players -> Tagged_GameState -> Color -> Maybe Move
searchOnPlayerColor players taggedGameState color =
    let
        mbSearchDepth = mbSearchDepthForPlayerColor players color
        taggedGameState' = setCurrentPlayerColorOn taggedGameState color
    in
        case mbSearchDepth of
            Nothing          -> Nothing            
            Just searchDepth -> mbBestNextMove searchDepth taggedGameState'


mbSearchDepthForPlayerColor :: Players -> Color -> Maybe SearchDepth
mbSearchDepthForPlayerColor players color =
    let
        (Player rec) = ofColor color players
    in
        case rec.type of
            Person {suggestionSearchDepth: x} -> Just x

            Computer strategy ->
                case strategy of 
                    RandomPick    -> Nothing                        
                    SearchDepth x -> Just x       


setCurrentPlayerColorOn :: Tagged_GameState -> Color -> Tagged_GameState
setCurrentPlayerColorOn taggedGameState color = 
    let
        (Core rec) = core_FromTaggedGameState taggedGameState
        core' = Core $ rec {currentPlayerColorForSearch = color}
    in
        swapCore taggedGameState core'
                 