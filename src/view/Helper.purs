module Helper
    ( gameState
    , gameStateOn
    , isGameStarted
    , isHistoryUndoable
    )
    where

import Prelude

import Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import GameHistory (GameHistory, undoHistoryOnce)
import GameState (Tagged_GameState)
import State (State)
import StatusStartRestart (Status_StartRestart(..))


gameState :: State -> Tagged_GameState
gameState state = 
    gameStateOn state.gameHistory      


gameStateOn :: GameHistory -> Tagged_GameState
gameStateOn x = 
    NE.last x    


isGameStarted :: State -> Boolean
isGameStarted state = 
    state.status_StartRestart /= NotStarted


isHistoryUndoable :: GameHistory -> Boolean
isHistoryUndoable x =
    isJust $ undoHistoryOnce x    