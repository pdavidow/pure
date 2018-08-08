module Query
    ( Query(..)
    )
    where

import DOM.Event.Event (Event)
import Disk (Color)
import DisplaySquare (FilledOpponent_DisplaySquare, Move_DisplaySquare)
import GameState (Tagged_GameState)
import Halogen as H
import Settings (EditPlayerTypeRec)   


data Query a
    = Init a
    
    | MouseEnter_StartStopButton a
    | MouseLeave_StartStopButton a

    | MouseEnter_MoveSquare Move_DisplaySquare a
    | MouseLeave_MoveSquare a
    | MouseDown_MoveSquare Move_DisplaySquare a
    | MouseUp_MoveSquare Move_DisplaySquare a  

    | MouseEnter_FilledOpponentSquare FilledOpponent_DisplaySquare a
    | MouseLeave_FilledOpponentSquare a  
    
    | MouseUp_Anywhere a

    | Click_FlipCounts a
    | Click_ComputerStep a    

    | Click_GameStartRestart a
    | Click_Confirm_Restart a
    | Click_Cancel_Restart a 

    | Click_Settings_Open a
    ----------- 
    | Click_Settings_Save a      
    | Click_Settings_Save_Confirm a
    | Click_Settings_Save_Cancel a    
    -----------       
    | Click_Settings_Cancel Boolean a  
    | Click_Settings_Cancel_Confirm a
    | Click_Settings_Cancel_Cancel a        
    -----------     
    | Click_Settings_Reset a       
    | Click_Settings_Reset_Confirm a
    | Click_Settings_Reset_Cancel a    
    -----------
    | ModifySettings Color (EditPlayerTypeRec -> EditPlayerTypeRec) a
    | Click_Settings_selectedColor Color a    
    -----------

    | Search_Prior Tagged_GameState (H.SubscribeStatus -> a)
    | Search_After Tagged_GameState (H.SubscribeStatus -> a)

    | Undo a    
    | PreventDefault Event a    