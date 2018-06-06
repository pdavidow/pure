module Test.Main where

import Prelude

import BlackWhite (BlackWhite(..))
import Board (Board, EmptySquare(..), FilledRow(FilledRow), Outflanks(..), Move(..), Tagged_Square(Tagged_FilledSquare), boardAt, boardSquaresColored, boardFromConfig, initialBoard, isFilledSquare, toPosition, validMoves, applyBoardMove, filledPositions, movePositionChoices, moveColor)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (fromRight)
import Data.List (List(Nil), fromFoldable, (:), head, index, length, toUnfoldable)
import Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Newtype (overF)
import Data.Record (equal)
import Data.Tuple (Tuple(..))
import Disk (Color(..))
import GameState (Tagged_State(..), MidState(..), actual_UnusedDiskCounts_FromTaggedState_BlackWhite, board_FromTaggedState, nextMoves_FromTaggedState)
import History (applyMoveOnHistory, makeHistory)
import Lib (mapTakeWhile)
import Partial.Unsafe (unsafePartial)
import Position (Position, PositionRec, PositionRow(..), makeValidPosition, positionRec, radiatingPositionRows)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import BlackWhite (BlackWhite(..), makeBlackWhite)


filledRowToPosRow :: FilledRow -> PositionRow
filledRowToPosRow (FilledRow xs) =
  PositionRow $ xs
      # map (\ x -> toPosition $ Tagged_FilledSquare x)


board_Figure2 :: Board 
board_Figure2 =
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


unsafeMidStateFrom :: Tagged_State -> MidState
unsafeMidStateFrom taggedState =
    unsafePartial $ case taggedState of
        Tagged_MidState x -> x


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
            let p1 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
            let p2 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
            let p3 = PositionRow (makeValidPosition {x:1, y:3} : Nil)
            let p4 = PositionRow (makeValidPosition {x:1, y:1} : makeValidPosition {x:1, y:1} : Nil)

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
            let board = initialBoard
            let pos = makeValidPosition {x:4, y:4}
            let elem = boardAt board pos

            Assert.equal (isFilledSquare elem) true

        test "validMoves Black initialBoard" do      
            let moves = validMoves Black initialBoard

            let (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
            let (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1
            let (Move _ (EmptySquare pos2 _) (Outflanks o2)) = unsafePartial $ fromJust $ index moves 2
            let (Move _ (EmptySquare pos3 _) (Outflanks o3)) = unsafePartial $ fromJust $ index moves 3

            let outflanks0 = map filledRowToPosRow o0
            let outflanks1 = map filledRowToPosRow o1
            let outflanks2 = map filledRowToPosRow o2
            let outflanks3 = map filledRowToPosRow o3
                
            Assert.equal' "4 moves" (length moves) 4

            Assert.assert "move 0" $ equal (positionRec pos0) {x:4, y:3}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:4, y:4}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:3, y:4}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:4, y:4}]]

            Assert.assert "move 2" $ equal (positionRec pos2) {x:6, y:5}
            Assert.equal' "outflanks 2" outflanks2 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:5}]]

            Assert.assert "move 3" $ equal (positionRec pos3) {x:5, y:6}
            Assert.equal' "outflanks 3" outflanks3 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:5}]]        

        test "validMoves Black board_Figure2" do      
            let moves = validMoves Black board_Figure2

            let (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
            let (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1
            let (Move _ (EmptySquare pos2 _) (Outflanks o2)) = unsafePartial $ fromJust $ index moves 2
            let (Move _ (EmptySquare pos3 _) (Outflanks o3)) = unsafePartial $ fromJust $ index moves 3

            let outflanks0 = map filledRowToPosRow o0
            let outflanks1 = map filledRowToPosRow o1
            let outflanks2 = map filledRowToPosRow o2
            let outflanks3 = map filledRowToPosRow o3
                
            Assert.equal' "4 moves" (length moves) 4

            Assert.assert "move 0" $ equal (positionRec pos0) {x:2, y:3}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:3, y:3}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:2, y:8}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:3, y:7}]]

            Assert.assert "move 2" $ equal (positionRec pos2) {x:7, y:6}
            Assert.equal' "outflanks 2" outflanks2 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:7, y:5}]]

            Assert.assert "move 3" $ equal (positionRec pos3) {x:8, y:6}
            Assert.equal' "outflanks 3" outflanks3 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:7, y:5}]]   

        test "validMoves White board_Figure2" do      
            let moves = validMoves White board_Figure2

            let (Move _ (EmptySquare pos0 _) (Outflanks o0)) = unsafePartial $ fromJust $ index moves 0
            let (Move _ (EmptySquare pos1 _) (Outflanks o1)) = unsafePartial $ fromJust $ index moves 1

            let outflanks0 = map filledRowToPosRow o0
            let outflanks1 = map filledRowToPosRow o1
                
            Assert.equal' "2 moves" (length moves) 2

            Assert.assert "move 0" $ equal (positionRec pos0) {x:4, y:2}
            Assert.equal' "outflanks 0" outflanks0 $ fromFoldable [PositionRow $ fromFoldable [makeValidPosition {x:5, y:3}, makeValidPosition {x:6, y:4}]]

            Assert.assert "move 1" $ equal (positionRec pos1) {x:7, y:3}
            Assert.equal' "outflanks 1" outflanks1 $ fromFoldable 
                [ PositionRow $ fromFoldable [makeValidPosition {x:6, y:3}, makeValidPosition {x:5, y:3}, makeValidPosition {x:4, y:3}]
                , PositionRow $ fromFoldable [makeValidPosition {x:7, y:4}]
                , PositionRow $ fromFoldable [makeValidPosition {x:6, y:4}, makeValidPosition {x:5, y:5}, makeValidPosition {x:4, y:6}]
                ]

        suite "apply: validMoves White board_Figure2" do    
            test "move0" do     
                let boardBefore = board_Figure2     
                let moves = validMoves White boardBefore
                let move0 = unsafePartial $ fromJust $ index moves 0
                let boardAfter = applyBoardMove move0 boardBefore       

                Assert.equal' "white positions" (filledPositions White boardAfter) $ fromFoldable
                    [ makeValidPosition {x:3, y:3}
                    , makeValidPosition {x:3, y:7}
                    , makeValidPosition {x:4, y:2}
                    , makeValidPosition {x:5, y:3}
                    , makeValidPosition {x:6, y:4}
                    , makeValidPosition {x:7, y:5}
                    ]   

                Assert.equal' "black positions" (filledPositions Black boardAfter) $ fromFoldable
                    [ makeValidPosition {x:4, y:3}
                    , makeValidPosition {x:4, y:6}
                    , makeValidPosition {x:5, y:5}
                    , makeValidPosition {x:6, y:3}
                    , makeValidPosition {x:7, y:4}
                    ]  
    
            test "move1" do     
                let boardBefore = board_Figure2     
                let moves = validMoves White boardBefore
                let move1 = unsafePartial $ fromJust $ index moves 1
                let boardAfter = applyBoardMove move1 boardBefore       

                Assert.equal' "white positions" (filledPositions White boardAfter) $ fromFoldable
                    [ makeValidPosition {x:3, y:3}
                    , makeValidPosition {x:3, y:7}
                    , makeValidPosition {x:4, y:3}
                    , makeValidPosition {x:4, y:6}
                    , makeValidPosition {x:5, y:3}
                    , makeValidPosition {x:5, y:5}
                    , makeValidPosition {x:6, y:3}
                    , makeValidPosition {x:6, y:4}
                    , makeValidPosition {x:7, y:3}
                    , makeValidPosition {x:7, y:4}
                    , makeValidPosition {x:7, y:5}                              
                    ]   

                Assert.equal' "black positions" (filledPositions Black boardAfter) $ fromFoldable 
                    []  

    suite "GameState" do
        test "start play" do
            let history1 = makeHistory

            let taggedState1 = NE.last history1
            let (BlackWhite {black: bp1, white: wp1}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState1
            let moves1 = nextMoves_FromTaggedState taggedState1
            let move1 = unsafePartial $ fromJust $ head moves1
            let history2 = unsafePartial $ fromRight $ applyMoveOnHistory move1 history1   

            let taggedState2 = NE.last history2
            let (BlackWhite {black: bp2, white: wp2}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState2
            let moves2 = nextMoves_FromTaggedState taggedState2
            let move2 = unsafePartial $ fromJust $ head moves2
            let history3 = unsafePartial $ fromRight $ applyMoveOnHistory move2 history2
            
            let taggedState3 = NE.last history3
            let (BlackWhite {black: bp3, white: wp3}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState3
            let moves3 = nextMoves_FromTaggedState taggedState3
            let move3 = unsafePartial $ fromJust $ head moves3
            let history4 = unsafePartial $ fromRight $ applyMoveOnHistory move3 history3
            
            let taggedState4 = NE.last history4    
            let (BlackWhite {black: bp4, white: wp4}) = filledPositions_BlackWhite $ board_FromTaggedState taggedState4     

            let numberedMovesWithPos1 = movePositionChoices moves1
            let numberedMovesWithPos2 = movePositionChoices moves2

            let (BlackWhite {black: bc1, white: wc1}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState1
            let (BlackWhite {black: bc2, white: wc2}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState2
            let (BlackWhite {black: bc3, white: wc3}) = actual_UnusedDiskCounts_FromTaggedState_BlackWhite taggedState3

            let (MidState {priorMove: priorMove2, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState2
            let (MidState {priorMove: priorMove3, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState3
            let (MidState {priorMove: priorMove4, status: _, nextMoves: _, core: _}) = unsafeMidStateFrom taggedState4

            Assert.equal' "initial unused disk counts" [bc1, wc1] [32, 32]         
            Assert.equal' "unused disk counts after 1st move (Black)" [bc2, wc2] [31, 32]  
            Assert.equal' "unused disk counts after 2nd move (White)" [bc3, wc3] [31, 31]  
            Assert.equal' "priorMoveColor for first 3 states after initial state" [moveColor priorMove2, moveColor priorMove3, moveColor priorMove4] [Black, White, Black]  

            Assert.equal' "filledPositions1" [bp1, wp1] [[[4,5],[5,4]], [[4,4],[5,5]]]
            Assert.equal' "filledPositions2" [bp2, wp2] [[[4,3],[4,4],[4,5],[5,4]], [[5,5]]]
            Assert.equal' "filledPositions3" [bp3, wp3] [[[4,3],[4,5],[5,4]], [[3,3],[4,4],[5,5]]]
            Assert.equal' "filledPositions4" [bp4, wp4] [[[2,3],[3,3],[4,3],[4,5],[5,4]], [[4,4],[5,5]]]
            

