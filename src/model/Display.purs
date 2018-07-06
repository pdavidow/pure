module Display
    ( Empty_NonMove_DisplaySquare(..)
    , Move_DisplaySquare(..)
    , FilledSelf_DisplaySquare(..)
    , FilledOpponent_DisplaySquare(..)
    , Empty_EndedGame_DisplaySquare(..)
    , Filled_EndedGame_DisplaySquare(..)
    , Tagged_DisplaySquare(..)
    , toDisplaySquare
    , toPosition
    , status
    )
    where

import Prelude

import Board as B
import Data.Lazy (Lazy, defer, force)
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..), fromJust)
import Disk (Color, toggleColor)
import GameState (MidGameState(..), EndedGameState(..), Tagged_GameState(..), EndStatus(..), MidStatus(..), Winner(..), mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState, winner)
import Partial.Unsafe (unsafePartial)
import Position (Position)
import Type.Data.Boolean (kind Boolean)
-- todo Arrays vs Lists ???

newtype Empty_NonMove_DisplaySquare = EmptyNonMove_DisplaySquare 
    { position :: Position
    } 

data Move_DisplaySquare = Move_DisplaySquare 
    { move :: B.Move
    , outflanks :: List Position
    }  

data FilledSelf_DisplaySquare = FilledSelf_DisplaySquare 
    { position :: Position
    , color :: Color
    }  

data FilledOpponent_DisplaySquare = FilledOpponent_DisplaySquare 
    { position :: Position
    , color :: Color
    , moves :: List Position    
    , outflanks :: List Position
    } 

newtype Empty_EndedGame_DisplaySquare = Empty_EndedGame_DisplaySquare 
    { position :: Position
    } 

data Filled_EndedGame_DisplaySquare = Filled_EndedGame_DisplaySquare 
    { position :: Position
    , color :: Color
    , mbIsWinningColor :: Maybe Boolean
    }      

data Tagged_DisplaySquare 
    = Tagged_Empty_NonMove_DisplaySquare Empty_NonMove_DisplaySquare
    | Tagged_Move_DisplaySquare Move_DisplaySquare
    | Tagged_FilledSelf_DisplaySquare FilledSelf_DisplaySquare
    | Tagged_FilledOpponent_DisplaySquare FilledOpponent_DisplaySquare
    | Tagged_Empty_EndedGame_DisplaySquare Empty_EndedGame_DisplaySquare
    | Tagged_Filled_EndedGame_DisplaySquare Filled_EndedGame_DisplaySquare    


instance eqMove_DisplaySquare :: Eq Move_DisplaySquare where
    eq (Move_DisplaySquare rec1) (Move_DisplaySquare rec2) = 
        B.movePosition rec1.move == B.movePosition rec2.move


toPosition :: Tagged_DisplaySquare -> Position
toPosition taggedDisplaySquare =
    case taggedDisplaySquare of
        Tagged_Empty_NonMove_DisplaySquare (EmptyNonMove_DisplaySquare rec)        -> rec.position
        Tagged_Move_DisplaySquare (Move_DisplaySquare rec)                         -> B.movePosition rec.move
        Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)             -> rec.position
        Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)     -> rec.position
        Tagged_Empty_EndedGame_DisplaySquare (Empty_EndedGame_DisplaySquare rec)   -> rec.position
        Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec) -> rec.position  


toDisplaySquare :: Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare taggedGameState taggedSquare =
    case taggedGameState of
        Tagged_StartGameState _ ->
            toDisplaySquare_NonEndedGame taggedGameState taggedSquare

        Tagged_MidGameState _ ->
            toDisplaySquare_NonEndedGame taggedGameState taggedSquare

        Tagged_EndedGameState x -> 
            toDisplaySquare_EndedGame x taggedSquare


toDisplaySquare_NonEndedGame :: Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_NonEndedGame taggedGameState taggedSquare =
    let
        moveColor = unsafePartial fromJust $ mbNextMoveColor_FromTaggedGameState taggedGameState
        moves = nextMoves_FromTaggedGameState taggedGameState
    in
        case taggedSquare of
            B.Tagged_EmptySquare _ ->
                let
                    mbMove = moves
                        # find (\ move -> (==) (B.toPosition taggedSquare) (B.movePosition move))
                in
                    case mbMove of
                        Nothing -> 
                            Tagged_Empty_NonMove_DisplaySquare $ EmptyNonMove_DisplaySquare 
                                { position: B.toPosition taggedSquare
                                }

                        Just move -> 
                            Tagged_Move_DisplaySquare $ Move_DisplaySquare 
                                { move: move
                                , outflanks: B.outflankPositions move
                                } 

            B.Tagged_FilledSquare x -> 
                case moveColor == B.filledSquareColor x of
                    true ->
                        Tagged_FilledSelf_DisplaySquare $ FilledSelf_DisplaySquare 
                            { position: B.toPosition taggedSquare
                            , color: moveColor
                            }  

                    false ->
                        let
                            position = B.toPosition taggedSquare
                            color = toggleColor moveColor
                            ({movePositions: movesForFilled, outflankPositions: outflanksForFilled}) = movesAndOutflanksForFilled position moves 
                        in
                            Tagged_FilledOpponent_DisplaySquare $ FilledOpponent_DisplaySquare 
                                { position: position
                                , color: color
                                , moves: movesForFilled                        
                                , outflanks: outflanksForFilled
                                }  
    

toDisplaySquare_EndedGame :: EndedGameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_EndedGame endedGameState taggedSquare =
    case taggedSquare of
        B.Tagged_EmptySquare _ -> -- perhaps possible
            Tagged_Empty_EndedGame_DisplaySquare $ Empty_EndedGame_DisplaySquare 
                { position: B.toPosition taggedSquare
                }

        B.Tagged_FilledSquare x -> 
            let
                color = B.filledSquareColor x
            in
                Tagged_Filled_EndedGame_DisplaySquare $ Filled_EndedGame_DisplaySquare
                    { position: B.toPosition taggedSquare
                    , color: color
                    , mbIsWinningColor: mbIsWinningColor color endedGameState
                    }


mbIsWinningColor :: Color -> EndedGameState -> Maybe Boolean
mbIsWinningColor color endedGameState =
    case winner endedGameState of
        WinColor winColor -> Just $ color == winColor
        Tie               -> Nothing


movesAndOutflanksForFilled :: Position -> List B.Move -> {movePositions :: List Position, outflankPositions :: List Position}
movesAndOutflanksForFilled position allMoves =     
    let
        moves = allMoves
            # filter (\ move -> elem position $ B.outflankPositions move)
            # nub 
            
        movePositions = moves
            # map B.movePosition

        outflankPositions = moves
            # concatMap (\ move -> B.outflankPositions_Traversing position move)
            # nub
    in
        { movePositions: movePositions
        , outflankPositions: outflankPositions
        }        


status :: Tagged_GameState -> String
status taggedGameState =
    let
        lzNextMoveColor :: Lazy Color
        lzNextMoveColor = defer $ \ _ -> unsafePartial fromJust $ mbNextMoveColor_FromTaggedGameState taggedGameState

        lzNextMoveColorStatusOn :: Color -> Lazy String
        lzNextMoveColorStatusOn color = defer $ \ _ -> (show color) <> " to move next"

        lzNextMoveColorStatus :: Lazy String
        lzNextMoveColorStatus = defer $ \ _ -> force $ lzNextMoveColorStatusOn $ force lzNextMoveColor
    in
        case taggedGameState of
            Tagged_StartGameState _ ->
                "Game Start, " <> force lzNextMoveColorStatus
            
            Tagged_MidGameState (MidGameState rec) ->
                case rec.status of
                    Normal -> 
                        force lzNextMoveColorStatus
                    
                    ForfeitTurn_Rule2 -> 
                        let
                            color = force lzNextMoveColor
                        in
                            (show $ toggleColor color) <> " forfeits Turn, " <> (force $ lzNextMoveColorStatusOn color)
                    
                    TransferDisk_Rule9 -> 
                        "Transfer Disk, " <> force lzNextMoveColorStatus

            Tagged_EndedGameState x -> 
                gameSummaryDisplay x


gameSummaryDisplay :: EndedGameState -> String
gameSummaryDisplay x@(EndedGameState rec) =
    let 
        reasonString = case rec.status of
            NoUnusedDisksForBoth -> "No more available disks for either player"
            NoValidMoves         -> "No more valid moves"

        winnerString = case winner x of
            WinColor color -> "Winner is " <> (show color)
            Tie            -> "TIE game"
    in 
        "GAME OVER (" <> reasonString <> ") " <> winnerString
