module GameState
    ( Core(..)
    , StartGameState(..)
    , MidGameState(..)
    , EndedGameState(..)
    , Tagged_GameState(..)
    , MidStatus(..)
    , EndStatus(..)
    , Winner(..)
    , NextMoves
    , makeStartGameState
    , board_FromTaggedGameState
    , core_FromTaggedGameState
    , mbPriorMove_FromTaggedGameState 
    , mbIsPriorMoveAtPosition
    , nextMoveColor_FromStartGameState
    , nextMoveColor_FromMidGameState
    , mbNextMoveColor_FromTaggedGameState
    , nextMoves_FromTaggedGameState
    , unusedDiskCounts_FromTaggedGameState
    , nextMovesFrom
    , applyMoveOnGameState
    , colorResultingInTaggedGameState
    , isOutflankOfPriorMove
    , isZeroUnusedDiskCount
    , isForfeitTurn
    , isStartGameState
    , isEndedGameState
    , winner
    , mbIsWinningColor
    , swapCore
    , makeStartGameStateOn -- todo only used in testing
    )
    where
      
import Prelude

import BlackWhite (BlackWhite(..))
import Board (Board, Move, Tagged_Square(..), applyBoardMove, boardArrayAt, initialBoard, isMoveAtPosition, outflankPositions, squaresColoredCounts_BlackWhite, toPosition, validMoves, moveColor, boardAt, filledSquares, toFilledSquare, isSquareColored, isEmptyAt, boardSquaresColored, cornerCounts_BlackWhite, filledSquaresAdjacentToEmptyCorners)
import Data.GameTree (class Node, Score(..))
import Data.Integral (fromIntegral)
import Data.List (List(..), concatMap, elem, filter, fromFoldable, length, mapMaybe, null, zip)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sum)
import Data.Tuple (Tuple(..))
import Disk (Color(..), toggleColor)
import Position (Position, isValidPositionRec, makeValidPosition, positionRec)
import UnusedDiskCount (UnusedDiskCounts, makeUnusedDiskCounts, transferDiskTo, decreaseByOneFor)

newtype Core = Core {unusedDiskCounts :: UnusedDiskCounts, board :: Board, currentPlayerColorForSearch :: Color}

newtype StartGameState = StartGameState {color :: Color, nextMoves :: NextMoves, core :: Core}

newtype MidGameState = MidGameState {priorMove :: Move, status :: MidStatus, nextMoves :: NextMoves, core :: Core}

newtype EndedGameState = EndedGameState {priorMove :: Move, status :: EndStatus, core :: Core}

type NextMoves = List Move

data Tagged_GameState
    = Tagged_StartGameState StartGameState
    | Tagged_MidGameState   MidGameState
    | Tagged_EndedGameState EndedGameState

data MidStatus
    = Normal
    | ForfeitTurn_Rule2
    | TransferDisk_Rule9

data EndStatus
    = NoUnusedDisksForBoth
    | NoValidMoves 

data Winner
    = WinColor Color
    | Tie

derive instance eqCore :: Eq Core    
derive instance eqStartGameState :: Eq StartGameState
derive instance eqMidGameState :: Eq MidGameState
derive instance eqEndedGameState :: Eq EndedGameState
derive instance eqTagged_GameState :: Eq Tagged_GameState
derive instance eqMidStatus :: Eq MidStatus
derive instance eqEndStatus :: Eq EndStatus

------------------

instance nodeTagged_GameState :: Node Tagged_GameState 
    where

    isTerminal :: Tagged_GameState -> Boolean
    isTerminal taggedGameState =
        case taggedGameState of
            Tagged_StartGameState _ -> false
            Tagged_MidGameState   _ -> false
            Tagged_EndedGameState _ -> true


    score :: Tagged_GameState -> Score
    score taggedGameState = 
        Score $ heuristic_Score taggedGameState


    children :: Tagged_GameState -> List Tagged_GameState
    children taggedGameState =
        nextMoves_FromTaggedGameState taggedGameState
            # map (\ move -> applyMoveOnGameState move taggedGameState)

------------------


makeStartGameStateOn :: Board -> StartGameState
makeStartGameStateOn board =
    StartGameState 
        { color: color
        , nextMoves: nextMovesFrom color board
        , core: Core 
            { unusedDiskCounts: makeUnusedDiskCounts
            , board: board
            , currentPlayerColorForSearch: color
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
                (Core coreRec) = core_FromTaggedGameState taggedGameState
                color = moveColor move
                unusedDiskCounts' = decreaseByOneFor color coreRec.unusedDiskCounts
                board' = applyBoardMove move coreRec.board
            in
                MidGameState 
                    { priorMove: move
                    , status: Normal
                    , nextMoves: nextMovesFrom (toggleColor color) board'
                    , core: Core 
                        { unusedDiskCounts: unusedDiskCounts'
                        , board: board'
                        , currentPlayerColorForSearch: coreRec.currentPlayerColorForSearch
                        }
                    }
    in
        case taggedGameState of
            Tagged_StartGameState _ -> processMidGameState makeMidGameState
            Tagged_MidGameState   _ -> processMidGameState makeMidGameState
            Tagged_EndedGameState _ -> taggedGameState -- should never get here    


processMidGameState :: MidGameState -> Tagged_GameState
processMidGameState midGameState@(MidGameState {priorMove: priorMove, status: _, nextMoves: nextMoves, core: core@(Core coreRec)}) =
    let
        unusedDiskCounts = coreRec.unusedDiskCounts
        board = coreRec.board

        priorColor = moveColor priorMove
        nextColor = toggleColor priorColor

        isZeroUnused_Prior = isZeroUnusedDiskCount priorColor core
        isZeroUnused_Next  = isZeroUnusedDiskCount  nextColor core

        nextMoves' = nextMovesFrom priorColor board
        
        end_NoValidMoves :: Tagged_GameState
        end_NoValidMoves = 
            Tagged_EndedGameState $ EndedGameState {priorMove: priorMove, status: NoValidMoves, core: core}
        
        end_NoUnusedDisksForBoth :: Tagged_GameState
        end_NoUnusedDisksForBoth = 
            Tagged_EndedGameState $ EndedGameState {priorMove: priorMove, status: NoUnusedDisksForBoth, core: core}

        forfeitTurn :: Tagged_GameState
        forfeitTurn = 
            Tagged_MidGameState $ MidGameState {priorMove: priorMove, status: ForfeitTurn_Rule2, nextMoves: nextMoves', core: core}

        transferDisk :: Tagged_GameState
        transferDisk = 
            Tagged_MidGameState $ MidGameState {priorMove: priorMove, status: TransferDisk_Rule9, nextMoves: nextMoves, core: Core {unusedDiskCounts: unusedDiskCounts', board: board, currentPlayerColorForSearch: coreRec.currentPlayerColorForSearch}}
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
        Tagged_EndedGameState (EndedGameState rec) -> rec.core


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
        Tagged_MidGameState (MidGameState rec)     -> moveColor rec.priorMove
        Tagged_EndedGameState (EndedGameState rec) -> moveColor rec.priorMove


mbNextMoveColor_FromTaggedGameState :: Tagged_GameState -> Maybe Color
mbNextMoveColor_FromTaggedGameState taggedGameState =  
    case taggedGameState of
        Tagged_StartGameState x -> Just $ nextMoveColor_FromStartGameState x
        Tagged_MidGameState x -> Just $ nextMoveColor_FromMidGameState x
        Tagged_EndedGameState _ -> Nothing        


nextMoveColor_FromStartGameState :: StartGameState -> Color
nextMoveColor_FromStartGameState (StartGameState rec) = 
    rec.color


nextMoveColor_FromMidGameState :: MidGameState -> Color
nextMoveColor_FromMidGameState (MidGameState rec) =  
    case rec.status of
        Normal -> toggleColor $ moveColor rec.priorMove
        ForfeitTurn_Rule2 -> moveColor rec.priorMove
        TransferDisk_Rule9 -> toggleColor $ moveColor rec.priorMove 


mbPriorMove_FromTaggedGameState :: Tagged_GameState -> Maybe Move
mbPriorMove_FromTaggedGameState taggedGameState =    
    case taggedGameState of
        Tagged_StartGameState _                    -> Nothing
        Tagged_MidGameState (MidGameState rec)     -> Just rec.priorMove
        Tagged_EndedGameState (EndedGameState rec) -> Just rec.priorMove               


mbIsPriorMoveAtPosition :: Tagged_GameState -> Position -> Maybe Boolean
mbIsPriorMoveAtPosition taggedGameState position =
    isMoveAtPosition position
        <$> 
            mbPriorMove_FromTaggedGameState taggedGameState


isOutflankOfPriorMove :: Tagged_GameState -> Position -> Boolean
isOutflankOfPriorMove taggedGameState pos =
    maybe 
        false 
        (\ move -> elem pos $ outflankPositions move) 
        (mbPriorMove_FromTaggedGameState taggedGameState)


nextMoves_FromTaggedGameState :: Tagged_GameState -> NextMoves
nextMoves_FromTaggedGameState taggedGameState =
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> rec.nextMoves
        Tagged_MidGameState (MidGameState rec)     -> rec.nextMoves
        Tagged_EndedGameState _                    -> Nil      


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

        Tagged_EndedGameState _ -> 
            false


isStartGameState :: Tagged_GameState -> Boolean
isStartGameState taggedGameState = 
    case taggedGameState of
        Tagged_StartGameState _ -> true
        Tagged_MidGameState   _ -> false
        Tagged_EndedGameState _ -> false


isEndedGameState :: Tagged_GameState -> Boolean
isEndedGameState taggedGameState = 
    case taggedGameState of
        Tagged_StartGameState _ -> false
        Tagged_MidGameState   _ -> false
        Tagged_EndedGameState _ -> true


winner :: EndedGameState -> Winner
winner x =  
    -- Rule 10: Disks are counted and the player with the majority of their color showing is the winner.
    
    let
        (BlackWhite {black: b, white: w}) = squaresColoredCounts_BlackWhite $ board_FromTaggedGameState $ Tagged_EndedGameState x
    in
        if b > w then
            WinColor Black
        else if w > b then
            WinColor White 
        else
            Tie


mbIsWinningColor :: Color -> EndedGameState -> Maybe Boolean
mbIsWinningColor color endedGameState =
    case winner endedGameState of
        WinColor winColor -> Just $ color == winColor
        Tie               -> Nothing


swapCore :: Tagged_GameState -> Core -> Tagged_GameState
swapCore taggedGameState core =
    case taggedGameState of
        Tagged_StartGameState (StartGameState rec) -> 
            Tagged_StartGameState $ StartGameState $ rec {core = core}

        Tagged_MidGameState (MidGameState rec) -> 
            Tagged_MidGameState $ MidGameState $ rec {core = core}

        Tagged_EndedGameState (EndedGameState rec) -> 
            Tagged_EndedGameState $ EndedGameState $ rec {core = core}


heuristic_Score :: Tagged_GameState -> Number 
heuristic_Score taggedGameState = 
    -- https://kartikkukreja.wordpress.com/2013/03/30/heuristic-function-for-reversiothello/
    -- Assume: boardSize == 8
    -- Negamax formulation

    case taggedGameState of
        Tagged_StartGameState _  -> 
            0.0          

        Tagged_MidGameState x -> 
            let
                nextMoveColor = nextMoveColor_FromMidGameState x
                nextMoves = nextMoves_FromTaggedGameState taggedGameState
                board = board_FromTaggedGameState taggedGameState   
                (Core coreRec) = core_FromTaggedGameState taggedGameState             
                score = 
                    (10.0 * heuristic_PieceDifference nextMoveColor board) + 
                        (801.724 * heuristic_CornerOccupancy nextMoveColor board) + 
                            (382.026 * heuristic_CornerCloseness nextMoveColor board) + 
                                (78.922 * heuristic_Mobility nextMoveColor nextMoves board) + 
                                    (74.396 * heuristic_FrontierDisks nextMoveColor board) + 
                                        (10.0 * heuristic_DiskSquares nextMoveColor board)                        
            in
                negamaxFilter nextMoveColor coreRec.currentPlayerColorForSearch score

        Tagged_EndedGameState x@(EndedGameState rec) -> 
            let
                finalMoveColor = toggleColor $ moveColor rec.priorMove 
                board = board_FromTaggedGameState $ Tagged_EndedGameState x
                (Core coreRec) = core_FromTaggedGameState taggedGameState 
                score = heuristic_PieceDifference finalMoveColor board
            in
                negamaxFilter finalMoveColor coreRec.currentPlayerColorForSearch score
    where

    negamaxFilter :: Color -> Color -> Number -> Number
    negamaxFilter colorOfInterest currentPlayerColorForSearch score  = 
        if (colorOfInterest == currentPlayerColorForSearch) then
            score
        else
            negate score
            

    heuristic_PieceDifference :: Color -> Board -> Number 
    heuristic_PieceDifference myColor board = 
        let
            (BlackWhite {black: b, white: w}) = squaresColoredCounts_BlackWhite board

            ( Tuple myCount opCount ) = 
                if myColor == Black then
                    Tuple b w
                else
                    Tuple w b

            total = myCount + opCount
        in
            if myCount > opCount then
                (fromIntegral (100 * myCount) / fromIntegral total)
            else if myCount < opCount then
                (fromIntegral (-100 * opCount) / fromIntegral total)
            else
                0.0

 
    heuristic_FrontierDisks :: Color -> Board -> Number 
    heuristic_FrontierDisks myColor board = 
        let
            filledSquares' = filledSquares board
                # map (positionRec <<< toPosition <<< Tagged_FilledSquare)
                # concatMap 
                    ( \ rec@({x: i, y: j}) -> 
                        let
                            xs = fromFoldable $ map (i + _) [-1, -1,  0,  1,  1,  1,  0, -1] 
                            ys = fromFoldable $ map (j + _) [ 0,  1,  1,  1,  0, -1, -1, -1]
                        in
                            zip xs ys
                                # map (\ (Tuple i' j') -> {x: i', y: j'})
                                # filter (\ rec' -> isValidPositionRec rec' && (isEmptyAt (makeValidPosition rec') board))
                                # mapMaybe (\ _ -> toFilledSquare $ boardAt board $ makeValidPosition rec)
                    )

            count = \ color -> length $ filter (isSquareColored color) filledSquares'

            myCount = count myColor
            opCount = count $ toggleColor myColor

            total = myCount + opCount
        in
            if myCount > opCount then
                fromIntegral (-100 * myCount) / fromIntegral total
            else if myCount < opCount then
                fromIntegral (100 * opCount) / fromIntegral total
            else
                0.0


    heuristic_DiskSquares :: Color -> Board -> Number 
    heuristic_DiskSquares myColor board = 
        let
            v = 
                [ [ 20, -3, 11,  8,  8, 11, -3, 20 ]
                , [ -3, -7, -4,  1,  1, -4, -7, -3 ]
                , [ 11, -4,  2,  2,  2,  2, -4, 11 ]
                , [  8,  1,  2, -3, -3,  2,  1,  8 ]
                , [  8,  1,  2, -3, -3,  2,  1,  8 ]
                , [ 11, -4,  2,  2,  2,  2, -4, 11 ]
                , [ -3, -7, -4,  1,  1, -4, -7, -3 ]
                , [ 20, -3, 11,  8,  8, 11, -3, 20 ]
                ]

            weight = \ color ->
                boardSquaresColored color board
                    # map (boardArrayAt v <<< positionRec <<< toPosition <<< Tagged_FilledSquare)
                    # sum
                    
            myWeight = weight myColor
            opWeight = weight $ toggleColor myColor
        in
            fromIntegral (myWeight - opWeight)


    heuristic_CornerOccupancy :: Color -> Board -> Number 
    heuristic_CornerOccupancy myColor board = 
        let       
            (BlackWhite {black: b, white: w}) = cornerCounts_BlackWhite board

            ( Tuple myCornerCount oppCornerCount ) =
                if myColor == Black then Tuple b w
                else Tuple w b
        in
            fromIntegral $ 25 * (myCornerCount - oppCornerCount)


    heuristic_CornerCloseness :: Color -> Board -> Number 
    heuristic_CornerCloseness myColor board = 
        let
            xs = filledSquaresAdjacentToEmptyCorners board
            count = \ color -> length $ filter (isSquareColored color) xs

            myCount = count myColor
            opCount = count $ toggleColor myColor
        in
            -12.5 * fromIntegral (myCount - opCount)


    heuristic_Mobility :: Color -> NextMoves -> Board -> Number 
    heuristic_Mobility myColor nextMoves board = 
        let
            opColor = toggleColor myColor

            myMoveCount = length nextMoves
            oppMoveCount = length $ validMoves opColor board

            total = myMoveCount + oppMoveCount
        in
            if myMoveCount > oppMoveCount then
                fromIntegral (100 *  myMoveCount) / fromIntegral total
            else if myMoveCount < oppMoveCount then
                fromIntegral (-100 * oppMoveCount) / fromIntegral total
            else
                0.0