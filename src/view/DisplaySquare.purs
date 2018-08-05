module DisplaySquare
    ( Empty_NotStartedGame_DisplaySquare(..)
    , Filled_NotStartedGame_DisplaySquare(..)
    , Empty_NonMove_DisplaySquare(..)
    , Move_DisplaySquare(..)
    , FilledSelf_DisplaySquare(..)
    , FilledOpponent_DisplaySquare(..)
    , Empty_EndedGame_DisplaySquare(..)
    , Filled_EndedGame_DisplaySquare(..)
    , Tagged_DisplaySquare(..)
    , toDisplaySquare
    , toPosition
    )
    where

import Prelude

import Board as B
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Disk (Color, toggleColor)
import GameState as GS
import Partial.Unsafe (unsafePartial)
import Position (Position)

-- todo: Use Record v1.0.0 `merge` for base filled of {color, flipCount}


newtype Empty_NotStartedGame_DisplaySquare = Empty_NotStartedGame_DisplaySquare 
    { position :: Position
    } 

newtype Filled_NotStartedGame_DisplaySquare = Filled_NotStartedGame_DisplaySquare 
    { position :: Position
    , color :: Color  
    }    

newtype Empty_NonMove_DisplaySquare = EmptyNonMove_DisplaySquare 
    { position :: Position
    } 

--   Empty_Move_DisplaySquare (more precisely)
newtype Move_DisplaySquare = Move_DisplaySquare 
    { move :: B.Move
    , outflanks :: List Position
    }  

newtype FilledSelf_DisplaySquare = FilledSelf_DisplaySquare 
    { position :: Position
    , color :: Color
    , flipCount :: Int

    ---------------------------------
    -- mutually exclusive (of course)
    , isPriorMove :: Boolean
    , isOutflankOfPriorMove :: Boolean
    ---------------------------------    
    }  

newtype FilledOpponent_DisplaySquare = FilledOpponent_DisplaySquare 
    { position :: Position
    , color :: Color
    , moves :: List Position    
    , outflanks :: List Position
    , flipCount :: Int    

    ---------------------------------
    -- mutually exclusive (of course)
    , isPriorMove :: Boolean
    , isOutflankOfPriorMove :: Boolean
    --------------------------------- 
    } 

newtype Empty_EndedGame_DisplaySquare = Empty_EndedGame_DisplaySquare 
    { position :: Position
    } 

newtype Filled_EndedGame_DisplaySquare = Filled_EndedGame_DisplaySquare 
    { position :: Position
    , color :: Color
    , mbIsWinningColor :: Maybe Boolean
    , flipCount :: Int  

    ---------------------------------
    -- mutually exclusive (of course)
    , isPriorMove :: Boolean
    , isOutflankOfPriorMove :: Boolean
    --------------------------------- 
    }      

data Tagged_DisplaySquare 
    = Tagged_Empty_NotStartedGame_DisplaySquare Empty_NotStartedGame_DisplaySquare
    | Tagged_Filled_NotStartedGame_DisplaySquare Filled_NotStartedGame_DisplaySquare
    | Tagged_Empty_NonMove_DisplaySquare Empty_NonMove_DisplaySquare
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
        Tagged_Empty_NotStartedGame_DisplaySquare (Empty_NotStartedGame_DisplaySquare rec)   -> rec.position
        Tagged_Filled_NotStartedGame_DisplaySquare (Filled_NotStartedGame_DisplaySquare rec) -> rec.position
        Tagged_Empty_NonMove_DisplaySquare (EmptyNonMove_DisplaySquare rec)                  -> rec.position
        Tagged_Move_DisplaySquare (Move_DisplaySquare rec)                                   -> B.movePosition rec.move
        Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)                       -> rec.position
        Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)               -> rec.position
        Tagged_Empty_EndedGame_DisplaySquare (Empty_EndedGame_DisplaySquare rec)             -> rec.position
        Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec)           -> rec.position  


toDisplaySquare :: GS.Tagged_GameState -> Boolean -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare taggedGameState isGameStarted taggedSquare =
    if isGameStarted then
        case taggedGameState of
            GS.Tagged_StartGameState _ ->
                toDisplaySquare_PlayGame taggedGameState taggedSquare

            GS.Tagged_MidGameState _ ->
                toDisplaySquare_PlayGame taggedGameState taggedSquare

            GS.Tagged_EndedGameState x -> 
                toDisplaySquare_EndedGame x taggedSquare
    else
        toDisplaySquare_NotStartedGame taggedGameState taggedSquare


toDisplaySquare_NotStartedGame :: GS.Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_NotStartedGame taggedGameState taggedSquare =
    case taggedSquare of
        B.Tagged_EmptySquare _ -> 
            Tagged_Empty_NotStartedGame_DisplaySquare $ Empty_NotStartedGame_DisplaySquare 
                { position: B.toPosition taggedSquare
                }

        B.Tagged_FilledSquare x -> 
            let
                color = B.filledSquareColor x
            in
                Tagged_Filled_NotStartedGame_DisplaySquare $ Filled_NotStartedGame_DisplaySquare
                    { position: B.toPosition taggedSquare
                    , color: color
                    }

toDisplaySquare_PlayGame :: GS.Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_PlayGame taggedGameState taggedSquare =
    let
        moveColor = unsafePartial fromJust $ GS.mbNextMoveColor_FromTaggedGameState taggedGameState
        moves = GS.nextMoves_FromTaggedGameState taggedGameState
    in
        case taggedSquare of
            B.Tagged_EmptySquare _ ->
                let
                    mbMove = moves
                        # find (\ move -> B.isMoveAtPosition (B.toPosition taggedSquare) move)
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
                let
                    position = B.toPosition taggedSquare
                    isPriorMove = fromMaybe false $ GS.mbIsPriorMoveAtPosition taggedGameState position  
                    isOutflankOfPriorMove = GS.isOutflankOfPriorMove taggedGameState position  
                in
                    case moveColor == B.filledSquareColor x of
                        true ->
                            Tagged_FilledSelf_DisplaySquare $ FilledSelf_DisplaySquare 
                                { position: position
                                , color: moveColor
                                , flipCount: B.filledSquareFlipCount x 
                                , isPriorMove: isPriorMove  
                                , isOutflankOfPriorMove: isOutflankOfPriorMove
                                }  

                        false ->
                            let
                                color = toggleColor moveColor
                                ({movePositions: movesForFilled, outflankPositions: outflanksForFilled}) = movesAndOutflanksForFilled position moves 
                            in
                                Tagged_FilledOpponent_DisplaySquare $ FilledOpponent_DisplaySquare 
                                    { position: position
                                    , color: color
                                    , moves: movesForFilled                        
                                    , outflanks: outflanksForFilled
                                    , flipCount: B.filledSquareFlipCount x
                                    , isPriorMove: isPriorMove   
                                    , isOutflankOfPriorMove: isOutflankOfPriorMove                                                                     
                                    }  
    

toDisplaySquare_EndedGame :: GS.EndedGameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_EndedGame endedGameState@(GS.EndedGameState rec) taggedSquare =
    case taggedSquare of
        B.Tagged_EmptySquare _ -> -- perhaps possible
            Tagged_Empty_EndedGame_DisplaySquare $ Empty_EndedGame_DisplaySquare 
                { position: B.toPosition taggedSquare
                }

        B.Tagged_FilledSquare x -> 
            let
                position = B.toPosition taggedSquare
                color = B.filledSquareColor x
            in
                Tagged_Filled_EndedGame_DisplaySquare $ Filled_EndedGame_DisplaySquare
                    { position: position
                    , color: color
                    , mbIsWinningColor: GS.mbIsWinningColor color endedGameState
                    , flipCount: B.filledSquareFlipCount x
                    , isPriorMove: B.isMoveAtPosition position rec.priorMove
                    , isOutflankOfPriorMove: GS.isOutflankOfPriorMove (GS.Tagged_EndedGameState endedGameState) position 
                    }


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


     