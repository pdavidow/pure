module Query
    (Query(..))
    where

import Display as DISP
import Disk (Color)
import DOM.Event.Event (Event)

data Query a
  = MouseEnter_StartStopButton a
  | MouseLeave_StartStopButton a
  | MouseEnter_MoveSquare DISP.Move_DisplaySquare a
  | MouseLeave_MoveSquare a
  | MouseDown_MoveSquare DISP.Move_DisplaySquare a
  | MouseUp_Anywhere a
  | MouseUp_MoveSquare DISP.Move_DisplaySquare a  
  | MouseEnter_FilledOpponentSquare DISP.FilledOpponent_DisplaySquare a
  | MouseLeave_FilledOpponentSquare a  
  | Click_FlipCounts a
  | Click_Open_Settings a
  | Click_Close_Settings a  
  | Click_Settings Color a
  | Click_Settings_Computer Color a
  | Click_Settings_Person Color a  
  | Click_GameStartRestart a
  | Click_Confirm_Restart a
  | Click_Cancel_Restart a 
  | Click_Confirm_ResetToDefaults a
  | Click_Cancel_ResetToDefaults a
  | Click_ComputerProceed a
  | Click_ResetSettingsToDefaults a
  | PreventDefault Event a
  | Undo a    