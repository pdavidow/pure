module Search
    ( SearchDepth(..)
    , mbBestNextMove
    , defaultSearchDepth
    )
    where
      
import Prelude
import Board (Move, dummyMove)
import Data.Maybe (Maybe(..))
import GameState (MidGameState(..), EndedGameState(..), Tagged_GameState(..))
import Data.GameTree (bestMove, minmax)


data SearchDepth 
    = SearchDepth_1
    | SearchDepth_2
    | SearchDepth_3
    | SearchDepth_4
    | SearchDepth_5
    | SearchDepth_6 
    | SearchDepth_7
    | SearchDepth_8
    | SearchDepth_9
    | SearchDepth_10


defaultSearchDepth :: SearchDepth
defaultSearchDepth =
    SearchDepth_3


depthLevel :: SearchDepth -> Int
depthLevel searchDepth =
    case searchDepth of
        SearchDepth_1  ->  1
        SearchDepth_2  ->  2
        SearchDepth_3  ->  3
        SearchDepth_4  ->  4
        SearchDepth_5  ->  5
        SearchDepth_6  ->  6
        SearchDepth_7  ->  7
        SearchDepth_8  ->  8
        SearchDepth_9  ->  9
        SearchDepth_10 -> 10


mbBestNextMove :: SearchDepth -> Tagged_GameState -> Maybe Move
mbBestNextMove searchDepth taggedGameState =
    case taggedGameState of
        Tagged_EndedGameState _ ->  
            Nothing -- should never get here

        _ -> 
            case bestMove (minmax $ depthLevel searchDepth) taggedGameState of
                Nothing                                       -> Nothing
                Just (Tagged_StartGameState _)                -> Nothing -- should never get here
                Just (Tagged_MidGameState (MidGameState rec)) -> Just rec.priorMove
                Just (Tagged_EndedGameState (EndedGameState rec)) -> Just rec.priorMove


-- todo unused?
bestNextMove :: SearchDepth -> Tagged_GameState -> Move
bestNextMove searchDepth taggedGameState =
    case mbBestNextMove searchDepth taggedGameState of
        Nothing   -> dummyMove
        Just move -> move