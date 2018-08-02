module DiskHTML
    ( diskClasses
    , diskChildren
    )
    where

import Prelude

import Data.Maybe (Maybe(..), fromJust, isJust)
import Disk (toggleColor)
import Display as DSP
import ClassConstants as CC
import GameState (mbNextMoveColor_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Partial.Unsafe (unsafePartial)
import Query (Query)
import State (State)
import Type.Data.Boolean (kind Boolean)


diskClasses :: State -> DSP.Tagged_DisplaySquare -> String
diskClasses state taggedDisplaySquare =
    case taggedDisplaySquare of 
        DSP.Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
            ""                 

        DSP.Tagged_Filled_NotStartedGame_DisplaySquare (DSP.Filled_NotStartedGame_DisplaySquare rec)  -> 
            if state.isImminentGameStart then
                DSP.placedDiskClassesForColor rec.color 
            else
                ""

        DSP.Tagged_Empty_NonMove_DisplaySquare _ ->
            ""

        DSP.Tagged_Move_DisplaySquare x ->   
            let
                mbMoveColor = mbNextMoveColor_FromTaggedGameState $ HLPR.gameStateOn state
            in   
                if HLPR.isMove_FocusedMoveSquare state x && isJust state.mb_MouseDown_MoveSquare then
                    DSP.potentialDiskClassesForColor $ unsafePartial fromJust $ mbMoveColor
                else
                    ""                            

        DSP.Tagged_FilledSelf_DisplaySquare (DSP.FilledSelf_DisplaySquare rec)  -> 
            DSP.placedDiskClassesForColor rec.color

        DSP.Tagged_FilledOpponent_DisplaySquare (DSP.FilledOpponent_DisplaySquare rec)  -> 
            if HLPR.isOutflankSquare_MouseDownMoveSquare state taggedDisplaySquare then
                DSP.flipDiskClassesForColor $ toggleColor rec.color
            else
                DSP.placedDiskClassesForColor $ rec.color 

        DSP.Tagged_Empty_EndedGame_DisplaySquare _ ->       
            ""                            
            
        DSP.Tagged_Filled_EndedGame_DisplaySquare (DSP.Filled_EndedGame_DisplaySquare rec) -> 
            DSP.placedDiskClassesForColor rec.color                    


diskChildren :: State -> DSP.Tagged_DisplaySquare -> Array (H.ComponentHTML Query)
diskChildren state taggedDisplaySquare = 
    let
        mbFlipCount = 
            if state.isShow_FlipCounts then
                case taggedDisplaySquare of 
                    DSP.Tagged_Empty_NotStartedGame_DisplaySquare _                                -> Nothing
                    DSP.Tagged_Filled_NotStartedGame_DisplaySquare _                               -> Nothing
                    DSP.Tagged_Empty_NonMove_DisplaySquare _                                       -> Nothing
                    DSP.Tagged_Move_DisplaySquare _                                                -> Nothing                                    
                    DSP.Tagged_FilledSelf_DisplaySquare (DSP.FilledSelf_DisplaySquare rec)             -> Just rec.flipCount
                    DSP.Tagged_FilledOpponent_DisplaySquare (DSP.FilledOpponent_DisplaySquare rec)     -> Just rec.flipCount
                    DSP.Tagged_Empty_EndedGame_DisplaySquare _                                     -> Nothing                                                         
                    DSP.Tagged_Filled_EndedGame_DisplaySquare (DSP.Filled_EndedGame_DisplaySquare rec) -> Just rec.flipCount
            else
                Nothing
    in
        case mbFlipCount of
            Nothing ->
                []
            Just flipCount ->
                [ HH.div
                    [ HP.classes [ HH.ClassName CC.flipCountText ]
                    ]
                    [ HH.text $ show flipCount  ]      
                ]    