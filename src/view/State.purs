module State
    ( State
    , initialState 
    )

    where

import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import DisplaySquare (Move_DisplaySquare)
import History (History, makeHistory)
import Position (Position)
import Settings (SettingsRec, defaultSettingsRec) 
import StatusStartRestart (Status_StartRestart(..))


type State = 
    { history :: History

    , mb_Focused_MoveSquare :: Maybe Move_DisplaySquare
    , mb_MouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
    , settings :: SettingsRec
    , isShow_FlipCounts :: Boolean
    , isBlockingOnSearch :: Boolean

    , isShowModal_Settings :: Boolean    
    , isShowModal_Confirm_Settings_Save :: Boolean
    , isShowModal_Confirm_Settings_Cancel :: Boolean    
    , isShowModal_Confirm_Settings_Reset :: Boolean

    , isImminentGameStart :: Boolean
    , isAwaitingConfirm_ResetSettingsToDefaults :: Boolean
    , status_StartRestart :: Status_StartRestart
    }      


initialState :: State 
initialState = 
    { history: makeHistory

    , mb_Focused_MoveSquare: Nothing
    , mb_MouseDown_MoveSquare: Nothing
    , outflanks_FocusedMoveSquare: Nil
    , moves_FocusedFilledOpponentSquare: Nil        
    , outflanks_FocusedFilledOpponentSquare: Nil
    , settings: defaultSettingsRec -- whatever    
    , isShow_FlipCounts: false
    , isBlockingOnSearch: false

    , isShowModal_Settings: false   
    , isShowModal_Confirm_Settings_Save: false
    , isShowModal_Confirm_Settings_Cancel: false 
    , isShowModal_Confirm_Settings_Reset: false

    , isImminentGameStart: false
    , isAwaitingConfirm_ResetSettingsToDefaults: false
    , status_StartRestart: NotStarted          
    }