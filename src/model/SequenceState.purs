module SequenceState
    ( SequenceState
    , initialSequenceState
    )
    where

import Board (Move)
import Data.Maybe (Maybe(Nothing))
import Player (Players)
import GameState (Tagged_GameState(Tagged_StartGameState), makeStartGameState)
import PlayerDefaults as DFLT


type SequenceState =
    { game :: Tagged_GameState
    , players :: Players
    , mbSuggestedMove :: Maybe Move
    }


initialSequenceState :: SequenceState
initialSequenceState =
    { game: Tagged_StartGameState makeStartGameState
    , players: DFLT.defaultPlayers
    , mbSuggestedMove: Nothing
    }      