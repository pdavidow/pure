module Test.Main where

import Prelude

import BlackWhite (BlackWhite(..), makeBlackWhite)
import Board (Board, EmptySquare(..), FilledRow(FilledRow), Outflanks(..), Move(..), Tagged_Square(Tagged_FilledSquare), boardAt, boardSquaresColored, boardFromConfig, initialBoard, isFilledSquare, toPosition, validMoves, applyBoardMove, movePositionChoices, moveColor, emptySquares, diskFrom, filledSquares) --, flipAt)   
import BoardSize (boardSize)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (tail)
import Data.Either (fromLeft, fromRight)
import Data.List (List(Nil), fromFoldable, (:), foldl, head, index, length, toUnfoldable, zip)
import Data.List.Lazy as LZ
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust)
import Data.Record (equal)
import Data.Tuple (Tuple(..))
import Disk (Color(..), flipCount, toggleColor)
import GameState (Tagged_State(..), Core(..), StartState(..), MidState(..), EndState(..), MidStatus(..), EndStatus(..), actual_UnusedDiskCounts_FromTaggedState_BlackWhite, board_FromTaggedState, nextMoves_FromTaggedState, mbNextMoveColor_FromTaggedState, nextMovesFrom, makeStartState, makeStartStateOn, isForfeitTurn) 
import History (MoveValidationError(..), applyMoveOnHistory, makeHistory, undoHistoryOnce)
import Lib (haskellRange, mapTakeWhile)
import Partial.Unsafe (unsafePartial)
import Position (Position, PositionRec, PositionRow(..), makeValidPosition, positionRec, radiatingPositionRows)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import UnusedDiskCount (decreaseByOneFor)


filledRowToPosRow :: FilledRow -> PositionRow
filledRowToPosRow (FilledRow xs) =
  PositionRow $ xs
      # map (\ x -> toPosition $ Tagged_FilledSquare x)


boardCustom1 :: Board 
boardCustom1 =
    -- Figure 2, from page 2 of http://www.boardgamecapital.com/game_rules/othello.pdf
    boardFromConfig 
        [ Tuple White $ makeValidPosition {x: 3,  y: 3}
        , Tuple White $ makeValidPosition {x: 3,  y: 7}
        , Tuple White $ makeValidPosition {x: 7,  y: 5}

        , Tuple Black $ makeValidPosition {x: 4,  y: 3}
        , Tuple Black $ makeValidPosition {x: 4,  y: 6}
        , Tuple Black $ makeValidPosition {x: 5,  y: 3}
        , Tuple Black $ makeValidPosition {x: 5,  y: 5}
        , Tuple Black $ makeValidPosition {x: 6,  y: 3}
        , Tuple Black $ makeValidPosition {x: 6,  y: 4}
        , Tuple Black $ makeValidPosition {x: 7,  y: 4}                            
        ]    


boardCustom2 :: Board 
boardCustom2 = -- full White board -- except for (1,1) which is Black, and (1,8) which is blank
    let
        first = 
            [ Tuple Black $ makeValidPosition {x: 1,  y: 1}
            , Tuple White $ makeValidPosition {x: 2,  y: boardSize}
            , Tuple White $ makeValidPosition {x: 3,  y: boardSize}
            , Tuple White $ makeValidPosition {x: 4,  y: boardSize}
            , Tuple White $ makeValidPosition {x: 5,  y: boardSize}
            , Tuple White $ makeValidPosition {x: 6,  y: boardSize}
            , Tuple White $ makeValidPosition {x: 7,  y: boardSize}  
            , Tuple White $ makeValidPosition {x: boardSize,  y: boardSize}                
            ]

        second = do  
            i <- haskellRange 1 boardSize
            j <- haskellRange 1 $ boardSize - 1
            pure (Tuple White $ makeValidPosition {x: i, y: j})
    in
        boardFromConfig $ first <> (unsafePartial $ fromJust $ tail second)


boardCustom3 :: Board 
boardCustom3 = -- full White board -- except for (1,1) which is Black, and last column which is blank
    let
        first = 
            [ Tuple Black $ makeValidPosition {x: 1,  y: 1}           
            ]

        second = do  
            i <- haskellRange 1 boardSize
            j <- haskellRange 1 $ boardSize - 1
            pure (Tuple White $ makeValidPosition {x: i, y: j})
    in
        boardFromConfig $ first <> (unsafePartial $ fromJust $ tail second)


boardCustom4 :: Board 
boardCustom4 =
    boardFromConfig $ do  
        i <- haskellRange 1 boardSize
        j <- haskellRange 1 $ boardSize - 1
        pure (Tuple White $ makeValidPosition {x: i, y: j})


unsafeMidStateFrom :: Tagged_State -> MidState
unsafeMidStateFrom taggedState =
    unsafePartial $ case taggedState of
        Tagged_MidState x -> x


unsafeEndStateFrom :: Tagged_State -> EndState
unsafeEndStateFrom taggedState =
    unsafePartial $ case taggedState of
        Tagged_EndState x -> x


filledPositions_BlackWhite :: Board -> BlackWhite (Array (Array Int))         
filledPositions_BlackWhite board =
    makeBlackWhite (f Black) (f White)
        where f = \ color -> toUnfoldable $ map (positionToArray <<< toPosition <<< Tagged_FilledSquare) $ boardSquaresColored color board


positionToArray :: Position -> Array Int
positionToArray pos =
    [x,y] where ({x:x, y:y}) = positionRec pos


main :: forall t1.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | t1
    )
    Unit
main = runTest do
    suite "Lib" do
        test "mapTakeWhile" do
            Assert.equal (mapTakeWhile (_ *2) (_ < 10) $ fromFoldable [1,2,3,4,5,6,7]) $ fromFoldable [2,4,6,8]

    suite "Position" do
        test "eqPositionRow" do
            let 
                p1 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
                p2 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
                p3 = PositionRow (makeValidPosition {x:1, y:3} : Nil)
                p4 = PositionRow (makeValidPosition {x:1, y:1} : makeValidPosition {x:1, y:1} : Nil)

            Assert.equal p1 p2
            Assert.expectFailure "" $ Assert.equal p1 p3
            Assert.expectFailure "" $ Assert.equal p1 p4

        test "radiatingPositionRows" do
            Assert.equal' "{x:1, y:1}" (radiatingPositionRows (makeValidPosition {x:1, y:1})) $ fromFoldable 
                [ PositionRow $ fromFoldable
                    [ makeValidPosition {x:2, y:1}
                    , makeValidPosition {x:3, y:1}
                    , makeValidPosition {x:4, y:1}
                    , makeValidPosition {x:5, y:1}
                    , makeValidPosition {x:6, y:1}
                    , makeValidPosition {x:7, y:1}
                    , makeValidPosition {x:8, y:1}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:1, y:2}
                    , makeValidPosition {x:1, y:3}
                    , makeValidPosition {x:1, y:4}
                    , makeValidPosition {x:1, y:5}
                    , makeValidPosition {x:1, y:6}
                    , makeValidPosition {x:1, y:7}
                    , makeValidPosition {x:1, y:8}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:2, y:2}
                    , makeValidPosition {x:3, y:3}
                    , makeValidPosition {x:4, y:4}
                    , makeValidPosition {x:5, y:5}
                    , makeValidPosition {x:6, y:6}
                    , makeValidPosition {x:7, y:7}
                    , makeValidPosition {x:8, y:8}
                    ]
                ]

            Assert.equal' "{x:1, y:8}" (radiatingPositionRows (makeValidPosition {x:1, y:8})) $ fromFoldable 
                [ PositionRow $ fromFoldable
                    [ makeValidPosition {x:2, y:8}
                    , makeValidPosition {x:3, y:8}
                    , makeValidPosition {x:4, y:8}
                    , makeValidPosition {x:5, y:8}
                    , makeValidPosition {x:6, y:8}
                    , makeValidPosition {x:7, y:8}
                    , makeValidPosition {x:8, y:8}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:1, y:7}
                    , makeValidPosition {x:1, y:6}
                    , makeValidPosition {x:1, y:5}
                    , makeValidPosition {x:1, y:4}
                    , makeValidPosition {x:1, y:3}
                    , makeValidPosition {x:1, y:2}
                    , makeValidPosition {x:1, y:1}            
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:2, y:7}
                    , makeValidPosition {x:3, y:6}
                    , makeValidPosition {x:4, y:5}
                    , makeValidPosition {x:5, y:4}
                    , makeValidPosition {x:6, y:3}
                    , makeValidPosition {x:7, y:2}
                    , makeValidPosition {x:8, y:1}
                    ]
                ]   

            Assert.equal' "{x:8, y:8}" (radiatingPositionRows (makeValidPosition {x:8, y:8})) $ fromFoldable 
                [ PositionRow $ fromFoldable
                    [ makeValidPosition {x:7, y:8}
                    , makeValidPosition {x:6, y:8}
                    , makeValidPosition {x:5, y:8}
                    , makeValidPosition {x:4, y:8}
                    , makeValidPosition {x:3, y:8}
                    , makeValidPosition {x:2, y:8}
                    , makeValidPosition {x:1, y:8}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:8, y:7}
                    , makeValidPosition {x:8, y:6}
                    , makeValidPosition {x:8, y:5}
                    , makeValidPosition {x:8, y:4}
                    , makeValidPosition {x:8, y:3}
                    , makeValidPosition {x:8, y:2}
                    , makeValidPosition {x:8, y:1}            
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:7, y:7}
                    , makeValidPosition {x:6, y:6}
                    , makeValidPosition {x:5, y:5}
                    , makeValidPosition {x:4, y:4}
                    , makeValidPosition {x:3, y:3}
                    , makeValidPosition {x:2, y:2}
                    , makeValidPosition {x:1, y:1}
                    ]
                ] 

            Assert.equal' "{x:8, y:1}" (radiatingPositionRows (makeValidPosition {x:8, y:1})) $ fromFoldable 
                [ PositionRow $ fromFoldable
                    [ makeValidPosition {x:7, y:1}
                    , makeValidPosition {x:6, y:1}
                    , makeValidPosition {x:5, y:1}
                    , makeValidPosition {x:4, y:1}
                    , makeValidPosition {x:3, y:1}
                    , makeValidPosition {x:2, y:1}
                    , makeValidPosition {x:1, y:1}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:8, y:2}
                    , makeValidPosition {x:8, y:3}
                    , makeValidPosition {x:8, y:4}
                    , makeValidPosition {x:8, y:5}
                    , makeValidPosition {x:8, y:6}
                    , makeValidPosition {x:8, y:7}
                    , makeValidPosition {x:8, y:8}            
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:7, y:2}
                    , makeValidPosition {x:6, y:3}
                    , makeValidPosition {x:5, y:4}
                    , makeValidPosition {x:4, y:5}
                    , makeValidPosition {x:3, y:6}
                    , makeValidPosition {x:2, y:7}
                    , makeValidPosition {x:1, y:8}
                    ]
                ]     

            Assert.equal' "{x:5, y:6}" (radiatingPositionRows (makeValidPosition {x:5, y:6})) $ fromFoldable 
                [ PositionRow $ fromFoldable
                    [ makeValidPosition {x:4, y:6}
                    , makeValidPosition {x:3, y:6}
                    , makeValidPosition {x:2, y:6}
                    , makeValidPosition {x:1, y:6}
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:6, y:6}
                    , makeValidPosition {x:7, y:6}
                    , makeValidPosition {x:8, y:6}
                    ]      
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:5, y:7}
                    , makeValidPosition {x:5, y:8}
                    ]                     
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:5, y:5}
                    , makeValidPosition {x:5, y:4}
                    , makeValidPosition {x:5, y:3}
                    , makeValidPosition {x:5, y:2}
                    , makeValidPosition {x:5, y:1}           
                    ]
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:4, y:7}
                    , makeValidPosition {x:3, y:8}
                    ]      
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:4, y:5}
                    , makeValidPosition {x:3, y:4}
                    , makeValidPosition {x:2, y:3}
                    , makeValidPosition {x:1, y:2}            
                    ]  
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:6, y:7}
                    , makeValidPosition {x:7, y:8}
                    ] 
                , PositionRow $ fromFoldable
                    [ makeValidPosition {x:6, y:5}
                    , makeValidPosition {x:7, y:4}
                    , makeValidPosition {x:8, y:3}
                    ]
                ] 

    suite "Board" do
        test "boardAt" do
            let 
                board = initialBoard
                pos = makeValidPosition {x:4, y:4}
                elem = boardAt board pos

            Assert.equal (isFilledSquare elem) true

        test "validMoves Black initialBoard" do      
            let 
                moves = validMoves Black initialBoard

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1
                (Move _ (EmptySquare pos2 _) (Outflanks o2)) = unsafePartial $ fromJust $ index moves 2
                (Move _ (EmptySquare pos3 _) (Outflanks o3)) = unsafePartial $ fromJust $ index moves 3

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
                outflanks2 = map filledRowToPosRow o2
                outflanks3 = map filledRowToPosRow o3
                
            Assert.equal' "4 moves" (length moves) 4

            Assert.assert "move 0" $ equal (positionRec pos0) {x:4, y:3}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:4, y:4}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:3, y:4}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:4, y:4}]]

            Assert.assert "move 2" $ equal (positionRec pos2) {x:6, y:5}
            Assert.equal' "outflanks 2" outflanks2 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:5}]]

            Assert.assert "move 3" $ equal (positionRec pos3) {x:5, y:6}
            Assert.equal' "outflanks 3" outflanks3 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:5}]]        

        test "validMoves Black boardCustom1" do      
            let 
                moves = validMoves Black boardCustom1

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1
                (Move _ (EmptySquare pos2 _) (Outflanks o2)) = unsafePartial $ fromJust $ index moves 2
                (Move _ (EmptySquare pos3 _) (Outflanks o3)) = unsafePartial $ fromJust $ index moves 3

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
                outflanks2 = map filledRowToPosRow o2
                outflanks3 = map filledRowToPosRow o3
                
            Assert.equal' "4 moves" (length moves) 4

            Assert.assert "move 0" $ equal (positionRec pos0) {x:2, y:3}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:3, y:3}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:2, y:8}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:3, y:7}]]

            Assert.assert "move 2" $ equal (positionRec pos2) {x:7, y:6}
            Assert.equal' "outflanks 2" outflanks2 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:7, y:5}]]

            Assert.assert "move 3" $ equal (positionRec pos3) {x:8, y:6}
            Assert.equal' "outflanks 3" outflanks3 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:7, y:5}]]   

        test "validMoves White boardCustom1" do      
            let 
                moves = validMoves White boardCustom1

                (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
                (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1

                outflanks0 = map filledRowToPosRow o0
                outflanks1 = map filledRowToPosRow o1
                
            Assert.equal' "2 moves" (length moves) 2

            Assert.assert "move 0" $ equal (positionRec pos0) {x:4, y:2}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:3}, makeValidPosition {x:6, y:4}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:7, y:3}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable 
                [ PositionRow $ fromFoldable [makeValidPosition {x:6, y:3}, makeValidPosition {x:5, y:3}, makeValidPosition {x:4, y:3}]
                , PositionRow $ fromFoldable [makeValidPosition {x:7, y:4}]
                , PositionRow $ fromFoldable [makeValidPosition {x:6, y:4}, makeValidPosition {x:5, y:5}, makeValidPosition {x:4, y:6}]
                ]

        suite "apply: validMoves White boardCustom1" do    
            test "move0" do     
                let 
                    board = boardCustom1     
                    moves = validMoves White board
                    move0 = unsafePartial $ fromJust $ index moves 0     
                    (BlackWhite {black: bp, white: wp}) = filledPositions_BlackWhite $ applyBoardMove move0 board 

                Assert.equal' "filledPositions" [bp, wp] 
                    [ [[4,3], [4,6], [5,5], [6,3], [7,4]]
                    , [[3,3], [3,7], [4,2], [5,3], [6,4], [7,5]]
                    ]
    
            test "move1" do     
                let 
                    board = boardCustom1     
                    moves = validMoves White board
                    move1 = unsafePartial $ fromJust $ index moves 1
                    (BlackWhite {black: bp, white: wp}) = filledPositions_BlackWhite $ applyBoardMove move1 board

                Assert.equal' "filledPositions" [bp, wp] 
                    [ []
                    , [[3,3], [3,7], [4,3], [4,6], [5,3], [5,5], [6,3], [6,4], [7,3], [7,4], [7,5]]
                    ]

    suite "GameState" do
        test "start play" do
            let 
                history1 = makeHistory

                taggedState1 = NE.last history1
                (BlackWhite {black: bp1, white: wp1}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState1
                moves1 = nextMoves_FromTaggedState taggedState1
                move1 = unsafePartial $ fromJust $ head moves1
                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1   

                taggedState2 = NE.last history2
                (BlackWhite {black: bp2, white: wp2}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState2
                moves2 = nextMoves_FromTaggedState taggedState2
                move2 = unsafePartial $ fromJust $ head moves2
                history3 = unsafePartial $ fromRight $ applyMoveOnHistory move2 history2
            
                taggedState3 = NE.last history3
                (BlackWhite {black: bp3, white: wp3}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState3
                moves3 = nextMoves_FromTaggedState taggedState3
                move3 = unsafePartial $ fromJust $ head moves3
                history4 = unsafePartial $ fromRight $ applyMoveOnHistory move3 history3
            
                taggedState4 = NE.last history4    
                (BlackWhite {black: bp4, white: wp4}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState4     

                numberedMovesWithPos1 = movePositionChoices moves1
                numberedMovesWithPos2 = movePositionChoices moves2

                (BlackWhite {black: bc1, white: wc1}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState1
                (BlackWhite {black: bc2, white: wc2}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState2
                (BlackWhite {black: bc3, white: wc3}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState3

                (MidState {priorMove: priorMove2, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState2
                (MidState {priorMove: priorMove3, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState3
                (MidState {priorMove: priorMove4, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState4

            Assert.equal' "initial unused disk counts" [bc1, wc1] [32, 32]         
            Assert.equal' "unused disk counts after 1st move (Black)" [bc2, wc2] [31, 32]  
            Assert.equal' "unused disk counts after 2nd move (White)" [bc3, wc3] [31, 31]  
            Assert.equal' "priorMoveColor for first 3 states after initial state" [moveColor priorMove2, moveColor priorMove3, moveColor priorMove4] [Black, White, Black]  

            Assert.equal' "filledPositions1" [bp1, wp1] 
                [ [[4,5], [5,4]]
                , [[4,4], [5,5]]
                ]

            Assert.equal' "filledPositions2" [bp2, wp2] 
                [ [[4,3], [4,4], [4,5], [5,4]]
                , [[5,5]]
                ]

            Assert.equal' "filledPositions3" [bp3, wp3] 
                [ [[4,3], [4,5], [5,4]]
                , [[3,3], [4,4], [5,5]]
                ]

            Assert.equal' "filledPositions4" [bp4, wp4] 
                [ [[2,3], [3,3], [4,3], [4,5], [5,4]]
                , [[4,4], [5,5]]
                ]
            
        test "Black uses very last disk on first move (contrived)" do      
            let 
                startState@(StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts board)}) = makeStartState
                (BlackWhite {black: bc, white: wc}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState
                unusedDiskCounts'  = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor Black) unusedDiskCounts) $ bc - 1
                unusedDiskCounts'' = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor White) unusedDiskCounts') $ wc

                taggedState1 = Tagged_StartState $ StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts'' board)}
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                moves1 = nextMoves_FromTaggedState taggedState1
                move1 = unsafePartial $ fromJust $ head moves1

                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1 
                (EndState {priorMove: _, status: endReason, core: _}) = unsafeEndStateFrom $ NE.last history2

            Assert.equal' "endReason" endReason NoUnusedDisksForBoth 

        test "Black on first move is confronted with full White board -- except for (1,1) which is Black, and (1,8) which is blank (contrived)" do
            let 
                taggedState1 = Tagged_StartState $ makeStartStateOn boardCustom2 
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                moves1 = nextMoves_FromTaggedState taggedState1
                move1 = unsafePartial $ fromJust $ head moves1

                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1 
                (EndState {priorMove: _, status: endReason, core: _}) = unsafeEndStateFrom $ NE.last history2

            Assert.equal' "endReason" endReason NoValidMoves 

        test "Black on first move is confronted with full White board -- except for (1,1) which is Black, and last column which is blank (contrived)" do
            let 
                taggedState1 = Tagged_StartState $ makeStartStateOn boardCustom3
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                moves1 = nextMoves_FromTaggedState taggedState1
                move1 = unsafePartial $ fromJust $ head moves1

                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1 
                midState@(MidState {priorMove: _, status: status, nextMoves: _, core: _}) = unsafeMidStateFrom $ NE.last history2     

            Assert.equal' "First move results in: Tagged_MidState, ForfeitTurn_Rule2" status ForfeitTurn_Rule2    
            Assert.equal' "Second move color is also Black" (mbNextMoveColor_FromTaggedState $ Tagged_MidState midState) $ Just Black

        test "White with no disks for his first move, is given one by Black (contrived)" do
            let 
                startState@(StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts board)}) = makeStartState
                (BlackWhite {black: bc, white: wc}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState
                unusedDiskCounts'  = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor White) unusedDiskCounts) wc 

                taggedState1 = Tagged_StartState $ StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts' board)}
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                moves1 = nextMoves_FromTaggedState taggedState1
                move1 = unsafePartial $ fromJust $ head moves1

                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1 
                taggedState2 = NE.last history2
                midState2@(MidState {priorMove: _, status: midStatus2, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState2     
                moves2 = nextMoves_FromTaggedState taggedState2
                move2 = unsafePartial $ fromJust $ head moves2
            
                history3 = unsafePartial $ fromRight $ applyMoveOnHistory move2 history2
                midState3@(MidState {priorMove: _, status: midStatus3, nextMoves: _, core: _}) = unsafeMidStateFrom $ NE.last history3    
           
                (BlackWhite {black: bc1, white: wc1}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState1
                (BlackWhite {black: bc2, white: wc2}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState2

            Assert.equal' "initial unused disk counts" 
                [bc1, wc1] [32, 0]         
            
            Assert.equal' "Black after using disk for his first move, then transfers another to White -- prior to White's first move" 
                [bc2, wc2] [30, 1]  

            Assert.equal' "Go from TransferDisk_Rule9 to Normal" 
                [midStatus2, midStatus3] [TransferDisk_Rule9, Normal]   

    suite "Validate Move" do
        test "NotOutflanking (contrived)" do
            let 
                board = boardCustom4

                taggedState1 = Tagged_StartState $ makeStartStateOn board
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                -- Black on first move is confronted with full White board -- except for last column which is blank
                move = Move Black (unsafePartial $ fromJust $ head $ emptySquares board) $ Outflanks Nil

                errors = unsafePartial $ fromLeft $ applyMoveOnHistory move history1    

            Assert.equal' "first move results in: NotOutflanking" 
                (NE.toUnfoldable errors) [NotOutflanking]

        test "NoAvailableDisk (contrived)" do
            let 
                startState@(StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts board)}) = makeStartState
                (BlackWhite {black: bc, white: wc}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState
                unusedDiskCounts' = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor Black) unusedDiskCounts) bc 

                taggedState1 = Tagged_StartState $ StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts' board)}
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                moves = nextMoves_FromTaggedState taggedState1
                -- Black on first move is confronted with no available disks
                move = unsafePartial $ fromJust $ head moves

                errors = unsafePartial $ fromLeft $ applyMoveOnHistory move history1    

            Assert.equal' "first move results in: NoAvailableDisk" 
                (NE.toUnfoldable errors) [NoAvailableDisk]

        test "WrongColor (contrived)" do
            let 
                history1 = makeHistory

                taggedState1 = NE.last history1
                (Move color e o) = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState1 
                move = Move (toggleColor color) e o

                errors = unsafePartial $ fromLeft $ applyMoveOnHistory move history1    

            Assert.equal' "first move results in: WrongColor" 
                (NE.toUnfoldable errors) [WrongColor]

        test "WrongColor, NoAvailableDisk, NotOutflanking (contrived)" do
            let 
                board' = boardCustom4
                startState@(StartState {color: c, nextMoves: n, core: (Core unusedDiskCounts board)}) = makeStartState
                (BlackWhite {black: bc, white: wc}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite $ Tagged_StartState startState
                unusedDiskCounts' = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor Black) unusedDiskCounts) bc 
                unusedDiskCounts'' = unsafePartial $ fromJust $ LZ.index (LZ.iterate (decreaseByOneFor White) unusedDiskCounts') wc 

                taggedState1 = Tagged_StartState $ StartState {color: c, nextMoves: (nextMovesFrom c board'), core: (Core unusedDiskCounts'' board')}
                history1 = unsafePartial $ fromJust $ NE.fromList $ taggedState1 : Nil
                move = Move White (unsafePartial $ fromJust $ head $ emptySquares board') $ Outflanks Nil

                errors = unsafePartial $ fromLeft $ applyMoveOnHistory move history1     

            Assert.equal' "first move results in: WrongColor, NoAvailableDisk, NotOutflanking" 
                (NE.toUnfoldable errors) [WrongColor, NoAvailableDisk, NotOutflanking]

        test "Undo (actual game)" do
            let 
                history1 = makeHistory

                taggedState1 = NE.last history1
                move1 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState1 -- black C4
                history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1 

                taggedState2 = NE.last history2
                move2 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState2 -- white C3
                history3 = unsafePartial $ fromRight $ applyMoveOnHistory move2 history2

                taggedState3 = NE.last history3
                move3 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState3 -- black C2
                history4 = unsafePartial $ fromRight $ applyMoveOnHistory move3 history3
            
                taggedState4 = NE.last history4
                move4 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState4 -- white B2
                history5 = unsafePartial $ fromRight $ applyMoveOnHistory move4 history4
            
                taggedState5 = NE.last history5
                move5 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState5 -- black A2
                history6 = unsafePartial $ fromRight $ applyMoveOnHistory move5 history5
            
                taggedState6 = NE.last history6
                move6 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState6 -- white A1
                history7 = unsafePartial $ fromRight $ applyMoveOnHistory move6 history6
            
                taggedState7 = NE.last history7
                move7 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState7 -- black D3
                history8 = unsafePartial $ fromRight $ applyMoveOnHistory move7 history7
            
                taggedState8 = NE.last history8
                move8 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState8 -- white A3
                history9 = unsafePartial $ fromRight $ applyMoveOnHistory move8 history8
            
                taggedState9 = NE.last history9
                move9 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState9 -- black B3
                history10 = unsafePartial $ fromRight $ applyMoveOnHistory move9 history9
            
                taggedState10 = NE.last history10
                move10 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState10 -- white D2
                history11 = unsafePartial $ fromRight $ applyMoveOnHistory move10 history10
            
                taggedState11 = NE.last history11
                move11 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState11 -- black B1
                history12 = unsafePartial $ fromRight $ applyMoveOnHistory move11 history11
            
                taggedState12 = NE.last history12
                move12 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState12 -- white C1
                history13 = unsafePartial $ fromRight $ applyMoveOnHistory move12 history12
            
                taggedState13 = NE.last history13
                move13 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState13 -- black D1
                history14 = unsafePartial $ fromRight $ applyMoveOnHistory move13 history13
            
                taggedState14 = NE.last history14
                move14 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState14 -- white E1
                history15 = unsafePartial $ fromRight $ applyMoveOnHistory move14 history14
            
                taggedState15 = NE.last history15
                move15 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState15 -- black E6
                history16 = unsafePartial $ fromRight $ applyMoveOnHistory move15 history15
            
                taggedState16 = NE.last history16
                move16 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState16 -- white E2
                history17 = unsafePartial $ fromRight $ applyMoveOnHistory move16 history16
            
                taggedState17 = NE.last history17
                move17 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState17 -- black F1
                history18 = unsafePartial $ fromRight $ applyMoveOnHistory move17 history17
            
                taggedState18 = NE.last history18
                move18 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState18 -- white F2
                history19 = unsafePartial $ fromRight $ applyMoveOnHistory move18 history18
            
                taggedState19 = NE.last history19
                move19 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState19 -- black F3
                history20 = unsafePartial $ fromRight $ applyMoveOnHistory move19 history19
            
                taggedState20 = NE.last history20
                move20 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState20 -- white G1
                history21 = unsafePartial $ fromRight $ applyMoveOnHistory move20 history20
            
                taggedState21 = NE.last history21 -- forfeit
                move21 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState21 -- white E3
                history22 = unsafePartial $ fromRight $ applyMoveOnHistory move21 history21
            
                taggedState22 = NE.last history22 -- forfeit
                move22 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState22 -- white F4
                history23 = unsafePartial $ fromRight $ applyMoveOnHistory move22 history22
            
                taggedState23 = NE.last history23
                move23 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState23 -- white G2
                history24 = unsafePartial $ fromRight $ applyMoveOnHistory move23 history23
            
                taggedState24 = NE.last history24
                move24 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState24 -- white G3
                history25 = unsafePartial $ fromRight $ applyMoveOnHistory move24 history24
            
                taggedState25 = NE.last history25
                move25 = unsafePartial $ fromJust $ head $ nextMoves_FromTaggedState taggedState25 -- black H1
                history26 = unsafePartial $ fromRight $ applyMoveOnHistory move25 history25         
            
            Assert.equal' "undo history1" (undoHistoryOnce history1) Nothing   
            Assert.equal' "undo history2" (undoHistoryOnce history2) Nothing 
            
            Assert.equal' "undo history3" (unsafePartial $ fromJust $ undoHistoryOnce history3) history1 
            Assert.equal' "undo history4" (unsafePartial $ fromJust $ undoHistoryOnce history4) history2
            Assert.equal' "undo history5" (unsafePartial $ fromJust $ undoHistoryOnce history5) history3   

            Assert.equal' "undo history21" (unsafePartial $ fromJust $ undoHistoryOnce history21) history20
            Assert.equal' "undo history22" (unsafePartial $ fromJust $ undoHistoryOnce history22) history21
            Assert.equal' "undo history23" (unsafePartial $ fromJust $ undoHistoryOnce history23) history19 
            Assert.equal' "undo history24" (unsafePartial $ fromJust $ undoHistoryOnce history24) history22
            Assert.equal' "undo history25" (unsafePartial $ fromJust $ undoHistoryOnce history25) history23    

            Assert.equal' "forfeits" (NE.filter isForfeitTurn history26) $ fromFoldable [taggedState21, taggedState22]                                                       

    -- suite "Commented Out" do
    --     test "flipAt" do
    --         let 
    --             board = initialBoard

    --             f :: Position -> Board -> Board
    --             f = \ pos board -> flipAt (boardAt board pos) board

    --             xs = filledSquares board

    --             board' = filledSquares board
    --                 # map (toPosition <<< Tagged_FilledSquare)
    --                 # zip (fromFoldable (haskellRange 1 $ length xs))
    --                 # foldl (\ acc ((Tuple i pos)) -> unsafePartial $ fromJust $ LZ.index (LZ.iterate (f pos) acc) i) board

    --             flippedCounts = filledSquares board'
    --                 # map (\ x -> flipCount $ diskFrom x)

    --         Assert.equal flippedCounts $ fromFoldable [1,2,3,4]
                                    
