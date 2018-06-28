module Display
    ( Empty_NonMove_DisplaySquare(..)
    , Move_DisplaySquare(..)
    , FilledSelf_DisplaySquare(..)
    , FilledOpponent_DisplaySquare(..)
    , Tagged_DisplaySquare(..)
    , toDisplaySquare
    , toPosition
    )
    where

import Prelude
import Board as B
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..))
import Disk (Color, toggleColor)
import Position (Position)

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

data Tagged_DisplaySquare 
    = Tagged_Empty_NonMove_DisplaySquare Empty_NonMove_DisplaySquare
    | Tagged_Move_DisplaySquare Move_DisplaySquare
    | Tagged_FilledSelf_DisplaySquare FilledSelf_DisplaySquare
    | Tagged_FilledOpponent_DisplaySquare FilledOpponent_DisplaySquare


instance eqMove_DisplaySquare :: Eq Move_DisplaySquare where
    eq (Move_DisplaySquare rec1) (Move_DisplaySquare rec2) = 
        B.movePosition rec1.move == B.movePosition rec2.move


toPosition :: Tagged_DisplaySquare -> Position
toPosition taggedDisplaySquare =
    case taggedDisplaySquare of
        Tagged_Empty_NonMove_DisplaySquare (EmptyNonMove_DisplaySquare rec)    -> rec.position
        Tagged_Move_DisplaySquare (Move_DisplaySquare rec)                     -> B.movePosition rec.move
        Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)         -> rec.position
        Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec) -> rec.position


toDisplaySquare :: Color -> List B.Move -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare moveColor moves taggedSquare =
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
                    Tagged_FilledOpponent_DisplaySquare $ FilledOpponent_DisplaySquare 
                        { position: position
                        , color: color
                        , moves: movesForFilled                        
                        , outflanks: outflanksForFilled
                        }                  
                    where

                    position = B.toPosition taggedSquare
                    color = toggleColor moveColor
                    ({movePositions: movesForFilled, outflankPositions: outflanksForFilled}) = movesAndOutflanksForFilled position moves 


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
        {movePositions: movePositions, outflankPositions: outflankPositions}        

 