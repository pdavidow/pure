module GameState
    ( Core(..)
    , StartGameState(..)
    , MidGameState(..)
    , EndGameState(..)
    , Tagged_GameState(..)
    , MidStatus(..)
    , EndStatus(..)
    , NextMoves
    , makeStartGameState
    , board_FromTaggedGameState
    , core_FromTaggedGameState
    , mbNextMoveColor_FromTaggedGameState
    , nextMoves_FromTaggedGameState
    , nextMovesFrom
    , applyMoveOnGameState
    , colorResultingInTaggedGameState
    , actual_UnusedDiskCounts_FromTaggedGameState_BlackWhite
    , isZeroUnusedDiskCount
    , isForfeitTurn
    , makeStartGameStateOn -- todo only used in testing
    )
    where
      
import Prelude      
import UnusedDiskCount (UnusedDiskCounts, Tagged_UnusedDiskCount(..), makeUnusedDiskCounts, isZeroCount, transferDiskTo, decreaseByOneFor, countFrom)
import Data.List (List(..), null)
import Board ( Board, Move(..), Tagged_Square(..), applyBoardMove, initialBoard, squaresColoredCounts_BlackWhite, validMoves, moveColor, boardAt, filledSquares, toFilledSquare, isSquareColored, isEmptyAt, boardSquaresColored, cornerCounts_BlackWhite, filledSquaresAdjacentToEmptyCorners ) 
import Disk (Color(..), toggleColor)
import BlackWhite (BlackWhite, BlackWhiteH(..), makeBlackWhite, makeBlackWhiteH)
import Data.Maybe (Maybe(..))

data Core = Core UnusedDiskCounts Board

data StartGameState = StartGameState {color :: Color, nextMoves :: NextMoves, core :: Core}

data MidGameState = MidGameState {priorMove :: Move, status :: MidStatus, nextMoves :: NextMoves, core :: Core}

data EndGameState = EndGameState {priorMove :: Move, status :: EndStatus, core :: Core}

type NextMoves = List Move

data Tagged_GameState
    = Tagged_StartGameState StartGameState
    | Tagged_MidGameState   MidGameState
    | Tagged_EndGameState   EndGameState

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


derive instance eqCore :: Eq Core    
derive instance eqStartGameState :: Eq StartGameState
derive instance eqMidGameState :: Eq MidGameState
derive instance eqEndGameState :: Eq EndGameState
derive instance eqTagged_GameState :: Eq Tagged_GameState
derive instance eqMidStatus :: Eq MidStatus
derive instance eqEndStatus :: Eq EndStatus


makeStartGameStateOn :: Board -> StartGameState
makeStartGameStateOn board =
    StartGameState 
        { color: color
        , nextMoves: nextMovesFrom color board
        , core: Core makeUnusedDiskCounts board
        } 
            where color = Black -- Rule 1: Black always moves first 


makeStartGameState :: StartGameState
makeStartGameState =
    makeStartGameStateOn initialBoard


nextMovesFrom :: Color -> Board -> NextMoves
nextMovesFrom color board =
    validMoves color board        


isZeroUnusedDiskCount :: Color -> Core -> Boolean
isZeroUnusedDiskCount color (Core (BlackWhiteH {black: b, white: w}) _) =
    case color of
        Black -> isZeroCount $ Tagged_BlackUnusedDiskCount b
        White -> isZeroCount $ Tagged_WhiteUnusedDiskCount w     


applyMoveOnGameState :: Move -> Tagged_GameState -> Tagged_GameState
applyMoveOnGameState move taggedGameState =
    let
        makeMidGameState :: MidGameState
        makeMidGameState =
            let
                (Core unusedDiskCounts board) = core_FromTaggedGameState taggedGameState
                color = moveColor move
                unusedDiskCounts' = decreaseByOneFor color unusedDiskCounts
                board' = applyBoardMove move board
            in
                MidGameState 
                    { priorMove: move
                    , status: Normal
                    , nextMoves: nextMovesFrom (toggleColor color) board'
                    , core: Core unusedDiskCounts' board'
                    }
    in
        case taggedGameState of
            Tagged_StartGameState _ -> processMidGameState makeMidGameState
            Tagged_MidGameState   _ -> processMidGameState makeMidGameState
            Tagged_EndGameState   _ -> taggedGameState -- should never get here    


processMidGameState :: MidGameState -> Tagged_GameState
processMidGameState midGameState@(MidGameState {priorMove: priorMove, status: _, nextMoves: nextMoves, core: core@(Core unusedDiskCounts board)}) =
    let
        priorColor = moveColor priorMove
        nextColor = toggleColor priorColor

        isZeroUnused_Prior = isZeroUnusedDiskCount priorColor core
        isZeroUnused_Next  = isZeroUnusedDiskCount  nextColor core

        nextMoves' = nextMovesFrom priorColor board
        
        end_NoValidMoves :: Tagged_GameState
        end_NoValidMoves = 
            Tagged_EndGameState $ EndGameState {priorMove: priorMove, status: NoValidMoves, core: core}
        
        end_NoUnusedDisksForBoth :: Tagged_GameState
        end_NoUnusedDisksForBoth = 
            Tagged_EndGameState $ EndGameState {priorMove: priorMove, status: NoUnusedDisksForBoth, core: core}

        forfeitTurn :: Tagged_GameState
        forfeitTurn = 
            Tagged_MidGameState $ MidGameState {priorMove: priorMove, status: ForfeitTurn_Rule2, nextMoves: nextMoves', core: core}

        transferDisk :: Tagged_GameState
        transferDisk = 
            Tagged_MidGameState $ MidGameState {priorMove: priorMove, status: TransferDisk_Rule9, nextMoves: nextMoves, core: Core unusedDiskCounts' board}
                where unusedDiskCounts' = transferDiskTo nextColor unusedDiskCounts

        passThru :: Tagged_GameState
        passThru = 
            Tagged_MidGameState midGameState
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


core_FromTaggedGameState :: Tagged_GameState -> Core
core_FromTaggedGameState taggedGameState =
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> rec.core
        Tagged_MidGameState (MidGameState rec)     -> rec.core
        Tagged_EndGameState (EndGameState rec)     -> rec.core


unusedDiskCounts_FromTaggedGameState :: Tagged_GameState -> UnusedDiskCounts
unusedDiskCounts_FromTaggedGameState taggedGameState =
    x where (Core x _) = core_FromTaggedGameState taggedGameState 


board_FromTaggedGameState :: Tagged_GameState -> Board
board_FromTaggedGameState taggedGameState =
    x where (Core _ x) = core_FromTaggedGameState taggedGameState        


colorResultingInTaggedGameState :: Tagged_GameState -> Color
colorResultingInTaggedGameState taggedGameState =
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> rec.color
        Tagged_MidGameState (MidGameState rec) -> moveColor rec.priorMove
        Tagged_EndGameState (EndGameState rec) -> moveColor rec.priorMove


mbNextMoveColor_FromTaggedGameState :: Tagged_GameState -> Maybe Color
mbNextMoveColor_FromTaggedGameState taggedGameState =  
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> Just rec.color
        Tagged_MidGameState midGameState -> Just $ nextMoveColor_FromMidGameState midGameState
        Tagged_EndGameState _ -> Nothing        
        

nextMoveColor_FromMidGameState :: MidGameState -> Color
nextMoveColor_FromMidGameState (MidGameState rec) =  
    case rec.status of
        Normal -> toggleColor $ moveColor rec.priorMove
        ForfeitTurn_Rule2 -> moveColor rec.priorMove
        TransferDisk_Rule9 -> toggleColor $ moveColor rec.priorMove 


mbPriorMove_FromTaggedGameState :: Tagged_GameState -> Maybe Move
mbPriorMove_FromTaggedGameState taggedGameState =    
    case taggedGameState of
        Tagged_StartGameState _                -> Nothing
        Tagged_MidGameState (MidGameState rec) -> Just rec.priorMove
        Tagged_EndGameState (EndGameState rec) -> Just rec.priorMove               


nextMoves_FromTaggedGameState :: Tagged_GameState -> NextMoves
nextMoves_FromTaggedGameState taggedGameState =
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> rec.nextMoves
        Tagged_MidGameState (MidGameState rec)     -> rec.nextMoves
        Tagged_EndGameState _                      -> Nil      
        
 
actual_UnusedDiskCounts_FromTaggedGameState_BlackWhite :: Tagged_GameState -> BlackWhite Int
actual_UnusedDiskCounts_FromTaggedGameState_BlackWhite taggedGameState =   
    makeBlackWhite (countFrom $ Tagged_BlackUnusedDiskCount b) (countFrom $ Tagged_WhiteUnusedDiskCount w)
       where (BlackWhiteH {black: b, white: w}) = unusedDiskCounts_FromTaggedGameState taggedGameState


isForfeitTurn :: Tagged_GameState -> Boolean
isForfeitTurn taggedGameState = 
    case taggedGameState of
        Tagged_StartGameState _ ->
            false

        Tagged_MidGameState (MidGameState rec) ->
            case rec.status of
                Normal -> false
                ForfeitTurn_Rule2 -> true
                TransferDisk_Rule9 -> false

        Tagged_EndGameState _ -> 
            false