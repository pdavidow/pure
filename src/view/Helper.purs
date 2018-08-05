module Helper
    ( gameStateOn
    , isGameStarted
    , isGameEnded
    , isHistoryUndoable
    , sequenceStateRecOn
    , sequenceStateOn
    )
    where
 
import Prelude

import Data.List.NonEmpty as NE 
import Data.Maybe (isJust)
import GameState (Tagged_GameState, isEndedGameState)
import History (History, undoHistoryOnce)
import SequenceState (SequenceStateRec, SequenceState, seqRec)
import State (State)
import StatusStartRestart (Status_StartRestart(..))


lastSequenceState :: History -> SequenceState
lastSequenceState history =
    NE.last history


lastSequenceStateRec :: History -> SequenceStateRec
lastSequenceStateRec history =
    seqRec $ lastSequenceState history


gameStateOn :: State -> Tagged_GameState
gameStateOn state = 
    (lastSequenceStateRec state.history).game    


sequenceStateOn :: State -> SequenceState
sequenceStateOn state = 
    NE.last state.history 


sequenceStateRecOn :: State -> SequenceStateRec
sequenceStateRecOn state = 
    seqRec $ sequenceStateOn state 


isGameStarted :: State -> Boolean
isGameStarted state = 
    state.status_StartRestart /= NotStarted


isGameEnded :: State -> Boolean
isGameEnded state = 
    isEndedGameState $ gameStateOn state


isHistoryUndoable :: History -> Boolean
isHistoryUndoable x =
    isJust $ undoHistoryOnce x