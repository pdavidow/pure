module Helper
    ( gameState
    , gameStateOn
    , isGameStarted
    , isHistoryUndoable
    , isMove_FocusedMoveSquare
    , isOutflankSquare_MouseDownMoveSquare
    , isOutflankSquare_FocusedMoveSquare
    , isSuggestedMoveSquare
    , isMove_FocusedFilledOpponentSquare
    , isOutflankSquare_FocusedFilledOpponentSquare
    )
    where

import Prelude

import Board (movePosition)
import Data.List (elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), isJust, maybe)
import Display as DSP
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


isMove_FocusedMoveSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isMove_FocusedMoveSquare state moveSquare =
    Just moveSquare == state.mb_Focused_MoveSquare     


isOutflankSquare_MouseDownMoveSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_MouseDownMoveSquare state taggedDisplaySquare =
    (isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare) && isJust state.mb_MouseDown_MoveSquare    



isOutflankSquare_FocusedMoveSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare =
    elem (DSP.toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare    



isSuggestedMoveSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isSuggestedMoveSquare state (DSP.Move_DisplaySquare rec) =
    maybe 
        false 
        (\ move -> move == rec.move) 
        state.mb_SuggestedMove


isMove_FocusedFilledOpponentSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isMove_FocusedFilledOpponentSquare state (DSP.Move_DisplaySquare rec) =
    elem (movePosition rec.move) state.moves_FocusedFilledOpponentSquare


isOutflankSquare_FocusedFilledOpponentSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare =
    elem (DSP.toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare    