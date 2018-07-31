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
import Settings (SettingsRec, toEditPlayers)    
import GameHistory (GameHistory, makeHistory)
import Player (Players)
import Position (Position)
import Sequencer (mbSuggestedMove)
import SettingsDefaults as DFLT
import StatusStartRestart (Status_StartRestart(..))


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

    , settings :: SettingsRec
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
    { players: players
    , gameHistory: gameHistory
    , mb_Focused_MoveSquare: Nothing
    , mb_MouseDown_MoveSquare: Nothing
    , outflanks_FocusedMoveSquare: Nil
    , moves_FocusedFilledOpponentSquare: Nil        
    , outflanks_FocusedFilledOpponentSquare: Nil
    , mb_SuggestedMove: mb_SuggestedMove
    , isShow_FlipCounts: false

    , settings: settings
    , isShowModal_Settings: false   
    , isShowModal_Confirm_Settings_Save: false
    , isShowModal_Confirm_Settings_Cancel: false 
    , isShowModal_Confirm_Settings_Reset: false

    , isImminentGameStart: false
    , isAwaitingConfirm_ResetSettingsToDefaults: false
    , status_StartRestart: NotStarted          
    }

    where 
        players = DFLT.defaultPlayers 

        settings = 
            { selectedColor: Black
            , players: toEditPlayers players
            }

        gameHistory = makeHistory 
        
        mb_SuggestedMove = mbSuggestedMove players gameHistory    