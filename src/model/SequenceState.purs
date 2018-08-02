module SequenceState
    ( SequenceStateRec 
    , SequenceState(..)
    , seqRec
    , initialSequenceState
    )
    where

import Prelude

import Board (Move)
import Data.Maybe (Maybe(Nothing))
import GameState (Tagged_GameState(Tagged_StartGameState), makeStartGameState)
import Player (Players)
import PlayerDefaults as DFLT


type SequenceStateRec =
    { game :: Tagged_GameState
    , players :: Players
    , mbSuggestedMove :: Maybe Move
    }

newtype SequenceState = SequenceState SequenceStateRec

derive instance eqSequenceState :: Eq SequenceState 

seqRec :: SequenceState -> SequenceStateRec
seqRec (SequenceState x) =
    x


initialSequenceState :: SequenceState
initialSequenceState =
    SequenceState
        { game: Tagged_StartGameState makeStartGameState
        , players: DFLT.defaultPlayers
        , mbSuggestedMove: Nothing
        }      