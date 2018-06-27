module Display
    ( Empty_NonMove_DisplaySquare(..)
    , Move_DisplaySquare(..)
    , Filled_DisplaySquare(..)
    , Tagged_DisplaySquare(..)
    , toDisplaySquare
    , toPosition
    )
    where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List, find)
import Board as B
import Disk (Color)
import Position (Position)
import GameState (NextMoves)

newtype Empty_NonMove_DisplaySquare = EmptyNonMove_DisplaySquare {position :: Position} 

data Move_DisplaySquare = Move_DisplaySquare {move :: B.Move, outflankPositions :: List Position}  

data Filled_DisplaySquare = Filled_DisplaySquare {position :: Position, color :: Color}  

data Tagged_DisplaySquare 
    = Tagged_Empty_NonMove_DisplaySquare Empty_NonMove_DisplaySquare
    | Tagged_Move_DisplaySquare Move_DisplaySquare
    | Tagged_Filled_DisplaySquare Filled_DisplaySquare


instance eqMove_DisplaySquare :: Eq Move_DisplaySquare where
    eq (Move_DisplaySquare rec1) (Move_DisplaySquare rec2) = 
        B.movePosition rec1.move == B.movePosition rec2.move


toPosition :: Tagged_DisplaySquare -> Position
toPosition taggedDisplaySquare =
    case taggedDisplaySquare of
        Tagged_Empty_NonMove_DisplaySquare (EmptyNonMove_DisplaySquare rec) -> rec.position
        Tagged_Move_DisplaySquare (Move_DisplaySquare rec)                  -> B.movePosition rec.move
        Tagged_Filled_DisplaySquare (Filled_DisplaySquare rec)              -> rec.position


toDisplaySquare :: NextMoves -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare moves taggedSquare =
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
                            , outflankPositions: B.outflankPositions move
                            } 

        B.Tagged_FilledSquare x -> 
            Tagged_Filled_DisplaySquare $ Filled_DisplaySquare 
                { position: B.toPosition taggedSquare
                , color: B.filledSquareColor x
                }  