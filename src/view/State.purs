module State
    ( State
    , initialState 
    )

    where
      

import Board (Move)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Disk (Color(..))
import Display (Move_DisplaySquare)
import GameHistory (GameHistory, makeHistory)
import Player (Players)
import Position (Position)
import StatusStartRestart (Status_StartRestart(..))
import SettingsDefaults as DFLT
import Sequencer (mbSuggestedMove)

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


initialState :: State 
initialState = 
    { players: players
    , gameHistory: gameHistory
    , mb_Focused_MoveSquare: Nothing
    , mb_MouseDown_MoveSquare: Nothing
    , outflanks_FocusedMoveSquare: Nil
    , moves_FocusedFilledOpponentSquare: Nil        
    , outflanks_FocusedFilledOpponentSquare: Nil
    , mb_SuggestedMove: mb_SuggestedMove
    , isShow_FlipCounts: false
    , isShow_ResetToDefaultsModal: false
    , isActive_SettingsModal: false
    , isImminentGameStart: false
    , isAwaitingConfirm_ResetSettingsToDefaults: false
    , status_StartRestart: NotStarted
    , activeSettingsColor: Black           
    }
    where 
        players = DFLT.defaultPlayers  
        gameHistory = makeHistory 
        mb_SuggestedMove = mbSuggestedMove players gameHistory    