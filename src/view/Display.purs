module Display
    ( potentialDiskClassesForColor
    , flipDiskClassesForColor
    , placedDiskClassesForColor
    , unusedDiskClassesForColor
    , gameOver_Emphasis
    , isActiveClass_Tag
    , isInvisibleClass_Tag
    , nameForStartRestartButton
    , isMove_FocusedMoveSquare 
    , isOutflankSquare_MouseDownMoveSquare 
    , isOutflankSquare_FocusedMoveSquare 
    , isSuggestedMoveSquare 
    , isMove_FocusedFilledOpponentSquare 
    , isOutflankSquare_FocusedFilledOpponentSquare      
    )
    where

import Prelude

import Board as B
import ClassConstants as CC
import Data.Lazy (Lazy, defer, force)
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Disk (Color(..), toggleColor)
import DisplaySquare as DSQ
import GameState as GS
import Helper as HLPR
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players)
import Position (Position)
import SequenceState (SequenceStateRec, seqRec)
import Sequencer (unsafe_CurrentPlayer, unsafe_OpponentPlayer)
import State (State)



potentialDiskClassesForColor :: Color -> String
potentialDiskClassesForColor color =
    case color of
        Black -> CC.potentialDisk_Black 
        White -> CC.potentialDisk_White


flipDiskClassesForColor :: Color -> String
flipDiskClassesForColor color =
    case color of
        Black -> CC.flipDisk_Black  
        White -> CC.flipDisk_White


placedDiskClassesForColor :: Color -> String
placedDiskClassesForColor color =
    case color of
        Black -> CC.placedDisk_Black
        White -> CC.placedDisk_White        


unusedDiskClassesForColor :: Color -> String
unusedDiskClassesForColor color =
    case color of
        Black -> CC.unusedDisk_Black
        White -> CC.unusedDisk_White    


gameOver_Emphasis :: GS.Tagged_GameState -> String
gameOver_Emphasis taggedGameState =       
    if GS.isEndedGameState taggedGameState then
        CC.gameOver_Emphasis
    else
        ""


isActiveClass_Tag :: Boolean -> String           
isActiveClass_Tag bool = 
    if bool then
        CC.isActive
    else
        ""
    

isInvisibleClass_Tag :: Boolean -> String           
isInvisibleClass_Tag bool = 
    if bool then
        CC.isInvisible 
    else
        ""
            
            
nameForStartRestartButton :: Boolean -> Players -> String
nameForStartRestartButton isGameStarted players =
    if isGameStarted then
        "Restart"
    else
        "Start" 
         

isMove_FocusedMoveSquare :: State -> DSQ.Move_DisplaySquare -> Boolean
isMove_FocusedMoveSquare state moveSquare =
    Just moveSquare == state.mb_Focused_MoveSquare     


isOutflankSquare_MouseDownMoveSquare :: State -> DSQ.Tagged_DisplaySquare -> Boolean
isOutflankSquare_MouseDownMoveSquare state taggedDisplaySquare =
    (isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare) && isJust state.mb_MouseDown_MoveSquare    



isOutflankSquare_FocusedMoveSquare :: State -> DSQ.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare =
    elem (DSQ.toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare    



isSuggestedMoveSquare :: State -> DSQ.Move_DisplaySquare -> Boolean
isSuggestedMoveSquare state (DSQ.Move_DisplaySquare rec) = 
    maybe 
        false 
        (\ move -> move == rec.move) 
        mbSuggestedMove
            where mbSuggestedMove = (seqRec (HLPR.sequenceStateOn state)).mbSuggestedMove   


isMove_FocusedFilledOpponentSquare :: State -> DSQ.Move_DisplaySquare -> Boolean
isMove_FocusedFilledOpponentSquare state (DSQ.Move_DisplaySquare rec) =
    elem (B.movePosition rec.move) state.moves_FocusedFilledOpponentSquare


isOutflankSquare_FocusedFilledOpponentSquare :: State -> DSQ.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare =
    elem (DSQ.toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare             