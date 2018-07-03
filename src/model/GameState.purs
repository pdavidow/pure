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
    , unusedDiskCounts_FromTaggedGameState
    , nextMovesFrom
    , applyMoveOnGameState
    , colorResultingInTaggedGameState
    , isZeroUnusedDiskCount
    , isForfeitTurn
    , makeStartGameStateOn -- todo only used in testing
    )
    where
      
import Prelude      
import UnusedDiskCount (UnusedDiskCounts, makeUnusedDiskCounts, transferDiskTo, decreaseByOneFor)
import Data.List (List(..), concatMap, filter, length, mapMaybe, null, zip)
import Board ( Board, Move(..), Tagged_Square(..), applyBoardMove, boardArrayAt, initialBoard, squaresColoredCounts_BlackWhite, toPosition, validMoves, moveColor, boardAt, filledSquares, toFilledSquare, isSquareColored, isEmptyAt, boardSquaresColored, cornerCounts_BlackWhite, filledSquaresAdjacentToEmptyCorners ) 
import Disk (Color(..), toggleColor)
import BlackWhite (BlackWhite(..), makeBlackWhite)
import Data.Maybe (Maybe(..))
import Data.Traversable (sum)
import Data.Tuple (Tuple(..))
import Position (isValidPositionRec, makeValidPosition, positionRec)
--import Data.Integral (fromIntegral)
--import Node (Node)


data Core = Core {unusedDiskCounts :: UnusedDiskCounts, board :: Board}

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

-- ------------------

-- instance Node Tagged_GameState 
--     where

--     isTerminal :: Tagged_GameState -> Boolean
--     isTerminal taggedGameState =
--         case taggedGameState of
--             Tagged_StartGameState _ -> false
--             Tagged_MidGameState   _ -> false
--             Tagged_EndGameState   _ -> true


--     score :: Tagged_GameState -> Score
--     score taggedGameState = 
--         Score $ round $ heuristic_score taggedGameState


--     children :: Tagged_GameState -> List Tagged_GameState
--     children taggedGameState =
--         nextMoves_FromTaggedGameState taggedGameState
--             # map (\ move -> applyMoveOnGameState move taggedGameState)

-- ------------------

makeStartGameStateOn :: Board -> StartGameState
makeStartGameStateOn board =
    StartGameState 
        { color: color
        , nextMoves: nextMovesFrom color board
        , core: Core 
            { unusedDiskCounts: makeUnusedDiskCounts
            , board: board
            }
        } 
        where color = Black -- Rule 1: Black always moves first 


makeStartGameState :: StartGameState
makeStartGameState =
    makeStartGameStateOn initialBoard


nextMovesFrom :: Color -> Board -> NextMoves
nextMovesFrom color board =
    validMoves color board        


isZeroUnusedDiskCount :: Color -> Core -> Boolean
isZeroUnusedDiskCount color (Core rec) =
    let
        (BlackWhite {black: b, white: w}) = rec.unusedDiskCounts
    in
        case color of
            Black -> b == 0
            White -> w == 0     


applyMoveOnGameState :: Move -> Tagged_GameState -> Tagged_GameState
applyMoveOnGameState move taggedGameState =
    let
        makeMidGameState :: MidGameState
        makeMidGameState =
            let
                (Core {unusedDiskCounts: unusedDiskCounts, board: board}) = core_FromTaggedGameState taggedGameState
                color = moveColor move
                unusedDiskCounts' = decreaseByOneFor color unusedDiskCounts
                board' = applyBoardMove move board
            in
                MidGameState 
                    { priorMove: move
                    , status: Normal
                    , nextMoves: nextMovesFrom (toggleColor color) board'
                    , core: Core 
                        { unusedDiskCounts: unusedDiskCounts'
                        , board: board'
                        }
                    }
    in
        case taggedGameState of
            Tagged_StartGameState _ -> processMidGameState makeMidGameState
            Tagged_MidGameState   _ -> processMidGameState makeMidGameState
            Tagged_EndGameState   _ -> taggedGameState -- should never get here    


processMidGameState :: MidGameState -> Tagged_GameState
processMidGameState midGameState@(MidGameState {priorMove: priorMove, status: _, nextMoves: nextMoves, core: core@(Core {unusedDiskCounts: unusedDiskCounts, board: board})}) =
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
            Tagged_MidGameState $ MidGameState {priorMove: priorMove, status: TransferDisk_Rule9, nextMoves: nextMoves, core: Core {unusedDiskCounts: unusedDiskCounts', board: board}}
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
    x where (Core {unusedDiskCounts: x, board: _}) = core_FromTaggedGameState taggedGameState 


board_FromTaggedGameState :: Tagged_GameState -> Board
board_FromTaggedGameState taggedGameState =
    x where (Core {unusedDiskCounts: _, board: x}) = core_FromTaggedGameState taggedGameState        


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

---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
-- Ideally should be in separate 'Heuristic' module, but that causes circular dependencies
---------------------------------------------------------------------------------------------

-- Assume: boardSize == 8

-- https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/

-- https://courses.cs.washington.edu/courses/cse573/04au/Project/mini1/RUSSIA/Final_Paper.pdf
    -- MaxPlayer to move next (section 4.1)

-- heuristic_pieceDifference :: Color -> Board -> Number 
-- heuristic_pieceDifference myColor board = 
--     let
--         (BlackWhite {black: b, white: w}) = squaresColoredCounts_BlackWhite board

--         ( Tuple myCount opCount ) = 
--             if myColor == Black then
--                 Tuple b w
--             else
--                 Tuple w b

--         total = myCount + opCount
--     in
--         if myCount > opCount then
--             100 * fromIntegral myCount / fromIntegral total
--         else if myCount < opCount then
--             -100 * fromIntegral opCount / fromIntegral total
--         else
--             0


-- heuristic_frontierDisks :: Color -> Board -> Number 
-- heuristic_frontierDisks myColor board = 
--     let
--         filledSquares' = filledSquares board
--             # map (positionRec <<< toPosition <<< Tagged_FilledSquare)
--             # concatMap 
--                 ( \ ({x: i, y: j}) -> 
--                     let
--                         xs = map (i + _) [-1, -1,  0,  1,  1,  1,  0, -1] 
--                         ys = map (j + _) [ 0,  1,  1,  1,  0, -1, -1, -1]
--                     in
--                         zip xs ys
--                             # filter (\ rec -> isValidPositionRec rec && (isEmptyAt (makeValidPosition rec) board))
--                             # mapMaybe (\ _ -> toFilledSquare $ boardAt board $ makeValidPosition i j)
--                 )

--         count = \ color -> length $ filter (isSquareColored color) filledSquares'

--         myCount = count myColor
--         opCount = count $ toggleColor myColor

--         total = myCount + opCount
--     in
--         if myCount > opCount then
--             -100 * fromIntegral myCount / fromIntegral total
--         else if myCount < opCount then
--             100 * fromIntegral opCount / fromIntegral total
--         else
--             0


-- heuristic_diskSquares :: Color -> Board -> Number 
-- heuristic_diskSquares myColor board = 
--     let
--         v = 
--             [ [ 20, -3, 11,  8,  8, 11, -3, 20 ]
--             , [ -3, -7, -4,  1,  1, -4, -7, -3 ]
--             , [ 11, -4,  2,  2,  2,  2, -4, 11 ]
--             , [  8,  1,  2, -3, -3,  2,  1,  8 ]
--             , [  8,  1,  2, -3, -3,  2,  1,  8 ]
--             , [ 11, -4,  2,  2,  2,  2, -4, 11 ]
--             , [ -3, -7, -4,  1,  1, -4, -7, -3 ]
--             , [ 20, -3, 11,  8,  8, 11, -3, 20 ]
--             ]

--         weight = \ color ->
--             boardSquaresColored color board
--                 # map (boardArrayAt v <<< positionRec <<< toPosition <<< Tagged_FilledSquare)
--                 # sum
                
--         myWeight = weight myColor
--         opWeight = weight $ toggleColor myColor
--     in
--         myWeight - opWeight


-- heuristic_cornerOccupancy :: Color -> Board -> Number 
-- heuristic_cornerOccupancy myColor board = 
--     let       
--         (BlackWhite {black: b, white: w}) = cornerCounts_BlackWhite board

--         ( Tuple myCornerCount oppCornerCount ) =
--             if myColor == Black then Tuple b w
--             else Tuple w b
--     in
--         25 * fromIntegral (myCornerCount - oppCornerCount)


-- heuristic_cornerCloseness :: Color -> Board -> Number 
-- heuristic_cornerCloseness myColor board = 
--     let
--         xs = filledSquaresAdjacentToEmptyCorners board
--         count = \ color -> length $ filter (isSquareColored color) xs

--         myCount = count myColor
--         opCount = count $ toggleColor myColor
--     in
--         -12.5 * fromIntegral (myCount - opCount)


-- heuristic_mobility :: Color -> NextMoves -> Board -> Number 
-- heuristic_mobility myColor nextMoves board = 
--     let
--         opColor = toggleColor myColor

--         myMoveCount = length nextMoves
--         oppMoveCount = length $ validMoves opColor board

--         total = myMoveCount + oppMoveCount
--     in
--         if myMoveCount > oppMoveCount then
--             100 * fromIntegral myMoveCount / fromIntegral total
--         else if myMoveCount < oppMoveCount then
--             -100 * fromIntegral oppMoveCount / fromIntegral total
--         else
--             0


-- heuristic_score :: Tagged_GameState -> Number 
-- heuristic_score taggedGameState = 
--     case taggedGameState of
--         Tagged_StartGameState _  -> 
--             1 -- constant whatever

--         Tagged_MidGameState midState -> 
--             let
--                 nextMoveColor = nextMoveColor_FromMidGameState midState
--                 nextMoves = nextMoves_FromTaggedGameState taggedGameState
--                 board = board_FromTaggedGameState taggedGameState
--             in
--                 (10 * heuristic_pieceDifference nextMoveColor board) + 
--                     (801.724 * heuristic_cornerOccupancy nextMoveColor board) + 
--                         (382.026 * heuristic_cornerCloseness nextMoveColor board) + 
--                             (78.922 * heuristic_mobility nextMoveColor nextMoves board) + 
--                                 (74.396 * heuristic_frontierDisks nextMoveColor board) + 
--                                     (10 * heuristic_diskSquares nextMoveColor board)                        

--         Tagged_EndGameState x@(EndGameState rec) -> 
--             let
--                 color = moveColor rec.priorMove 
--                 board = board_FromTaggedGameState x
--             in
--                 heuristic_pieceDifference (toggleColor color) board


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------            