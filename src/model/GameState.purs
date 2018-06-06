module GameState
    ( Core(..)
    , StartState(..)
    , MidState(..)
    , EndState(..)
    , Tagged_State(..)
    , MidStatus(..)
    , EndStatus(..)
    , NextMoves
    , makeStartState
    , board_FromTaggedState
    , core_FromTaggedState
    , mbNextMoveColor_FromTaggedState
    , nextMoves_FromTaggedState
    , applyMoveOnState
    , colorResultingInTaggedState
    , actual_UnusedDiskCounts_FromTaggedState_BlackWhite
    , isZeroUnusedDiskCount
    , isForfeitTurn
    )
    where
      
import Prelude      
import UnusedDiskCount (UnusedDiskCounts, Tagged_UnusedDiskCount(..), makeBlackUnusedDiskCount, makeWhiteUnusedDiskCount, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom)
import Data.List (List(..), null)
import Board ( Board, Move(..), Tagged_Square(..), applyBoardMove, initialBoard, squaresColoredCounts_BlackWhite, validMoves, moveColor, boardAt, filledSquares, toFilledSquare, isSquareColored, isEmptyAt, boardSquaresColored, cornerCounts_BlackWhite, filledSquaresAdjacentToEmptyCorners ) 
import Disk (Color(..), toggleColor)
import BlackWhite (BlackWhite, BlackWhiteH(..), makeBlackWhite, makeBlackWhiteH)
import Data.Maybe (Maybe(..))

data Core = Core UnusedDiskCounts Board

data StartState = StartState {color :: Color, nextMoves :: NextMoves, core :: Core}

data MidState = MidState {priorMove :: Move, status :: MidStatus, nextMoves :: NextMoves, core :: Core}

data EndState = EndState {priorMove :: Move, status :: EndStatus, core :: Core}

type NextMoves = List Move

data Tagged_State
    = Tagged_StartState StartState
    | Tagged_MidState   MidState
    | Tagged_EndState   EndState

data MidStatus
    = Normal
    | ForfeitTurn_Rule2
    | TransferDisk_Rule9

data EndStatus
    = NoUnusedDisksForBoth
    | NoValidMoves

-- data GameSummary = GameSummary EndStatus (BlackWhite SquareCount)   

data Winner
    = WinnerColor Color
    | Tie


makeStartState :: StartState
makeStartState =
    let
        color = Black -- Rule 1: Black always moves first.
        board = initialBoard
        nextMoves = nextMovesFrom color board
        core = Core (makeBlackWhiteH makeBlackUnusedDiskCount makeWhiteUnusedDiskCount) board
    in
        StartState {color: color , nextMoves: nextMoves, core: core} 



nextMovesFrom :: Color -> Board -> NextMoves
nextMovesFrom color board =
    validMoves color board        


isZeroUnusedDiskCount :: Color -> Core -> Boolean
isZeroUnusedDiskCount color (Core (BlackWhiteH {black: b, white: w}) _) =
    case color of
        Black -> isZeroCount $ Tagged_BlackUnusedDiskCount b
        White -> isZeroCount $ Tagged_WhiteUnusedDiskCount w     


applyMoveOnState :: Move -> Tagged_State -> Tagged_State
applyMoveOnState move taggedState =
    let
        makeMidState :: MidState
        makeMidState =
            let
                (Core unusedDiskCounts board) = core_FromTaggedState taggedState
                color = moveColor move
                unusedDiskCounts' = decreaseByOneFor color unusedDiskCounts
                board' = applyBoardMove move board
            in
                MidState 
                    { priorMove: move
                    , status: Normal
                    , nextMoves: nextMovesFrom (toggleColor color) board'
                    , core: Core unusedDiskCounts' board'
                    }
    in
        case taggedState of
            Tagged_StartState _ -> processMidState makeMidState
            Tagged_MidState   _ -> processMidState makeMidState
            Tagged_EndState   _ -> taggedState -- should never get here    


processMidState :: MidState -> Tagged_State
processMidState midState@(MidState {priorMove: priorMove, status: _, nextMoves: nextMoves, core: core@(Core unusedDiskCounts board)}) =
    let
        priorColor = moveColor priorMove
        nextColor = toggleColor priorColor

        isZeroUnused_Prior = isZeroUnusedDiskCount priorColor core
        isZeroUnused_Next  = isZeroUnusedDiskCount  nextColor core

        nextMoves' = nextMovesFrom priorColor board
        
        end_NoValidMoves :: Tagged_State
        end_NoValidMoves = 
            Tagged_EndState $ EndState {priorMove: priorMove, status: NoValidMoves, core: core}
        
        end_NoUnusedDisksForBoth :: Tagged_State
        end_NoUnusedDisksForBoth = 
            Tagged_EndState $ EndState {priorMove: priorMove, status: NoUnusedDisksForBoth, core: core}

        forfeitTurn :: Tagged_State
        forfeitTurn = 
            Tagged_MidState $ MidState {priorMove: priorMove, status: ForfeitTurn_Rule2, nextMoves: nextMoves', core: core}

        transferDisk :: Tagged_State
        transferDisk = 
            Tagged_MidState $ MidState {priorMove: priorMove, status: TransferDisk_Rule9, nextMoves: nextMoves, core: Core unusedDiskCounts' board}
                where unusedDiskCounts' = transferDiskTo nextColor unusedDiskCounts

        passThru :: Tagged_State
        passThru = 
            Tagged_MidState midState
    in
        if null nextMoves then 
            if null nextMoves' then
                end_NoValidMoves -- Rule 10: When it is no longer possible for either player to move, the game is over.
            else
                forfeitTurn -- Rule 2: If a player cannot outflank and flip at least one opposing disk, they forfeit their turn and their opponent moves again. 
        else if isZeroUnused_Next then
            if isZeroUnused_Prior then 
                end_NoUnusedDisksForBoth -- Rule 10: When it is no longer possible for either player to move, the game is over.
            else 
                transferDisk -- Rule 9: If a player runs out of disks, but still has the opportunity to outflank an opposing disk on their turn, the opponent must give the player a disk to use. This can happen as many times as the player needs and can use a disk.
        else
            passThru               


core_FromTaggedState :: Tagged_State -> Core
core_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState {color: _, nextMoves: _, core: x}) -> x
        Tagged_MidState (MidState {priorMove: _, status: _, nextMoves: _, core: x}) -> x
        Tagged_EndState (EndState {priorMove: _, status: _, core: x}) -> x


unusedDiskCounts_FromTaggedState :: Tagged_State -> UnusedDiskCounts
unusedDiskCounts_FromTaggedState taggedState =
    x where (Core x _) = core_FromTaggedState taggedState 


board_FromTaggedState :: Tagged_State -> Board
board_FromTaggedState taggedState =
    x where (Core _ x) = core_FromTaggedState taggedState        


colorResultingInTaggedState :: Tagged_State -> Color
colorResultingInTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState {color: x, nextMoves: _, core: _}) -> x
        Tagged_MidState (MidState {priorMove: (Move x _ _), status: _, nextMoves: _, core: _}) -> x
        Tagged_EndState (EndState {priorMove: (Move x _ _), status: _, core: _}) -> x  


mbNextMoveColor_FromTaggedState :: Tagged_State -> Maybe Color
mbNextMoveColor_FromTaggedState taggedState =  
    case taggedState of
        Tagged_StartState (StartState {color: x, nextMoves: _, core: _}) -> Just x
        Tagged_MidState midState -> Just $ nextMoveColor_FromMidState midState
        Tagged_EndState _ -> Nothing        
        

nextMoveColor_FromMidState :: MidState -> Color
nextMoveColor_FromMidState (MidState {priorMove: priorMove, status: status, nextMoves: _, core: _}) =  
    case status of
        Normal -> toggleColor $ moveColor priorMove
        ForfeitTurn_Rule2 -> moveColor priorMove
        TransferDisk_Rule9 -> toggleColor $ moveColor priorMove 


mbPriorMove_FromTaggedState :: Tagged_State -> Maybe Move
mbPriorMove_FromTaggedState taggedState =    
    case taggedState of
        Tagged_StartState _ -> Nothing
        Tagged_MidState (MidState {priorMove: x, status: _, nextMoves: _, core: _}) -> Just x
        Tagged_EndState (EndState {priorMove: x, status: _, core: _}) -> Just x               


nextMoves_FromTaggedState :: Tagged_State -> NextMoves
nextMoves_FromTaggedState taggedState =
    case taggedState of
        Tagged_StartState (StartState {color: _, nextMoves: x, core: _}) -> x
        Tagged_MidState (MidState {priorMove: _, status: _, nextMoves: x, core: _}) -> x
        Tagged_EndState _ -> Nil      
        
 
actual_UnusedDiskCounts_FromTaggedState_BlackWhite :: Tagged_State -> BlackWhite Int
actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState =   
    makeBlackWhite (countFrom $ Tagged_BlackUnusedDiskCount b) (countFrom $ Tagged_WhiteUnusedDiskCount w)
       where (BlackWhiteH {black: b, white: w}) = unusedDiskCounts_FromTaggedState taggedState


isForfeitTurn :: Tagged_State -> Boolean
isForfeitTurn taggedState = 
    case taggedState of
        Tagged_StartState _ ->
            false

        Tagged_MidState (MidState {priorMove: _, status: status, nextMoves: _, core: _}) ->
            case status of
                Normal -> false
                ForfeitTurn_Rule2 -> true
                TransferDisk_Rule9 -> false

        Tagged_EndState _ -> 
            false       
     