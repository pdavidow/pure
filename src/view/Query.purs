module Query
    ( Query(..)
    )
    where

import DOM.Event.Event (Event)
import Disk (Color)
import Display as DISP
import EditSetting (EditPlayerTypeRec) 


data Query a
    = MouseEnter_StartStopButton a
    | MouseLeave_StartStopButton a

    | MouseEnter_MoveSquare DISP.Move_DisplaySquare a
    | MouseLeave_MoveSquare a
    | MouseDown_MoveSquare DISP.Move_DisplaySquare a
    | MouseUp_MoveSquare DISP.Move_DisplaySquare a  

    | MouseEnter_FilledOpponentSquare DISP.FilledOpponent_DisplaySquare a
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
    | Click_Settings_PlayerColor Color a    
    -----------

    | Undo a    
    | PreventDefault Event a    