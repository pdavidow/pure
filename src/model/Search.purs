module Search
    ( SearchDepth(..)
    , mbBestNextMove
    , searchDepths
    , depthLevel
    )
    where
      
import Prelude

import Board (Move)
import Data.GameTree (bestMove, alphaBeta)
import Data.Maybe (Maybe(..))
import GameState (MidGameState(..), EndedGameState(..), Tagged_GameState(..)) 


data SearchDepth 
    = SearchDepth_1
    | SearchDepth_2
    | SearchDepth_3
    | SearchDepth_4
    | SearchDepth_5


derive instance eqSearchDepth :: Eq SearchDepth

searchDepths :: Array SearchDepth
searchDepths = 
    [ SearchDepth_1
    , SearchDepth_2
    , SearchDepth_3
    , SearchDepth_4
    , SearchDepth_5
    ]


depthLevel :: SearchDepth -> Int
depthLevel searchDepth =
    case searchDepth of
        SearchDepth_1  ->  1
        SearchDepth_2  ->  2
        SearchDepth_3  ->  3
        SearchDepth_4  ->  4
        SearchDepth_5  ->  5


mbBestNextMove :: SearchDepth -> Tagged_GameState -> Maybe Move
mbBestNextMove searchDepth taggedGameState =
    case taggedGameState of
        Tagged_EndedGameState _ ->  
            Nothing -- should never get here

        _ -> 
            case bestMove (alphaBeta $ depthLevel searchDepth) taggedGameState of
                Nothing                                           -> Nothing
                Just (Tagged_StartGameState _)                    -> Nothing -- should never get here
                Just (Tagged_MidGameState (MidGameState rec))     -> Just rec.priorMove
                Just (Tagged_EndedGameState (EndedGameState rec)) -> Just rec.priorMove