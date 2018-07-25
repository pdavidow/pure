module State
    ( State )

    where
      

import Board (Move)
import Data.List (List)
import Data.Maybe (Maybe)
import Disk (Color)
import Display (Move_DisplaySquare)
import GameHistory (GameHistory)
import Player (Players)
import Position (Position)
import StatusStartRestart (Status_StartRestart)


type State = 
    { players :: Players
    , gameHistory :: GameHistory
    , mb_Focused_MoveSquare :: Maybe Move_DisplaySquare
    , mb_MouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
    , mb_SuggestedMove :: Maybe Move
    , isShow_FlipCounts :: Boolean
    , isShow_ResetToDefaultsModal :: Boolean
    , isActive_SettingsModal :: Boolean
    , isImminentGameStart :: Boolean
    , isAwaitingConfirm_ResetSettingsToDefaults :: Boolean
    , status_StartRestart :: Status_StartRestart
    , activeSettingsColor :: Color
    }      