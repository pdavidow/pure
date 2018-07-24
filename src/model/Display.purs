module Display
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
    , placedDisksStatus
    , status
    , potentialDiskClassesForColor
    , flipDiskClassesForColor
    , placedDiskClassesForColor
    , unusedDiskClassesForColor
    , gameOver_Emphasis
    , isActiveClass_Tag
    , nameForStartRestartButton
    )
    where

import Prelude

import Board as B
import Data.Lazy (Lazy, defer, force)
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..), fromJust)
import Disk (Color(..), toggleColor)
import DisplayConstants as DC
import GameState (MidGameState(..), EndedGameState(..), Tagged_GameState(..), EndStatus(..), MidStatus(..), Winner(..), board_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState, isEndedGameState, winner)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players)
import Position (Position)
import Sequencer (unsafe_CurrentPlayer, unsafe_OpponentPlayer)
-- todo Arrays vs Lists ???
-- todo: Use Record v1.0.0 `merge` for base filled of {color, flipCount}


newtype Empty_NotStartedGame_DisplaySquare = Empty_NotStartedGame_DisplaySquare 
    { position :: Position
    } 

data Filled_NotStartedGame_DisplaySquare = Filled_NotStartedGame_DisplaySquare 
    { position :: Position
    , color :: Color  
    }    

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
    , flipCount :: Int
    }  

data FilledOpponent_DisplaySquare = FilledOpponent_DisplaySquare 
    { position :: Position
    , color :: Color
    , moves :: List Position    
    , outflanks :: List Position
    , flipCount :: Int    
    } 

newtype Empty_EndedGame_DisplaySquare = Empty_EndedGame_DisplaySquare 
    { position :: Position
    } 

data Filled_EndedGame_DisplaySquare = Filled_EndedGame_DisplaySquare 
    { position :: Position
    , color :: Color
    , mbIsWinningColor :: Maybe Boolean
    , flipCount :: Int    
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


toDisplaySquare :: Tagged_GameState -> Boolean -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare taggedGameState isGameStarted taggedSquare =
    if isGameStarted then
        case taggedGameState of
            Tagged_StartGameState _ ->
                toDisplaySquare_PlayGame taggedGameState taggedSquare

            Tagged_MidGameState _ ->
                toDisplaySquare_PlayGame taggedGameState taggedSquare

            Tagged_EndedGameState x -> 
                toDisplaySquare_EndedGame x taggedSquare
    else
        toDisplaySquare_NotStartedGame taggedGameState taggedSquare


toDisplaySquare_NotStartedGame :: Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
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

toDisplaySquare_PlayGame :: Tagged_GameState -> B.Tagged_Square -> Tagged_DisplaySquare
toDisplaySquare_PlayGame taggedGameState taggedSquare =
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
                            , flipCount: B.filledSquareFlipCount x 
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
                                , flipCount: B.filledSquareFlipCount x
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
                    , flipCount: B.filledSquareFlipCount x
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


status :: Boolean -> Boolean -> Players -> Tagged_GameState -> String
status isImminentGameStart isGameStarted players taggedGameState =
    let
        lzCurrentPlayer' :: Lazy Player
        lzCurrentPlayer' = defer $ \_ -> unsafe_CurrentPlayer players taggedGameState 

        lzOpponentPlayer' :: Lazy Player
        lzOpponentPlayer' = defer $ \_ -> unsafe_OpponentPlayer players taggedGameState    

        lzFirstMoveStatus :: Player -> Lazy String
        lzFirstMoveStatus player = defer $ \_ -> playerStatus player <> " to move first"  

        lzNextMoveStatus :: Player -> Lazy String
        lzNextMoveStatus player = defer $ \_ -> playerStatus player <> " to move next"                                                        
    in
        if isImminentGameStart || isGameStarted then
            case taggedGameState of
                Tagged_StartGameState _ -> force $ lzFirstMoveStatus $ force lzCurrentPlayer'

                Tagged_MidGameState (MidGameState rec) ->
                    case rec.status of
                        Normal -> force $ lzNextMoveStatus $ force lzCurrentPlayer'                       
                        ForfeitTurn_Rule2 ->  (force $ lzNextMoveStatus $ force lzCurrentPlayer') <> " (" <> (playerStatus $ force lzOpponentPlayer') <> " forfeits turn)"     
                        TransferDisk_Rule9 -> "TRANSFER UNUSED-DISK, " <> (force $ lzNextMoveStatus $ force lzCurrentPlayer')  

                Tagged_EndedGameState x -> gameSummaryDisplay x
        else
            ""


playerStatus :: Player -> String
playerStatus (Player color playerType) = 
    let
        suffix = show color
    in
        case playerType of
            Person   _ -> "Person " <> suffix
            Computer _ -> "Computer " <> suffix  


placedDiskCountsStatus :: Tagged_GameState -> String
placedDiskCountsStatus taggedGameState =
    board_FromTaggedGameState taggedGameState
        # B.squaresColoredCounts_BlackWhite
        # show 


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


potentialDiskClassesForColor :: Color -> String
potentialDiskClassesForColor color =
    case color of
        Black -> DC.potentialDisk_Black 
        White -> DC.potentialDisk_White


flipDiskClassesForColor :: Color -> String
flipDiskClassesForColor color =
    case color of
        Black -> DC.flipDisk_Black  
        White -> DC.flipDisk_White


placedDiskClassesForColor :: Color -> String
placedDiskClassesForColor color =
    case color of
        Black -> DC.placedDisk_Black
        White -> DC.placedDisk_White        


unusedDiskClassesForColor :: Color -> String
unusedDiskClassesForColor color =
    case color of
        Black -> DC.unusedDisk_Black
        White -> DC.unusedDisk_White    


gameOver_Emphasis :: Tagged_GameState -> String
gameOver_Emphasis taggedGameState =       
    if isEndedGameState taggedGameState then
        DC.gameOver_Emphasis
    else
        ""

isActiveClass_Tag :: Boolean -> String           
isActiveClass_Tag bool = 
    if bool then
        DC.isActive
    else
        ""
    

placedDisksStatus :: Boolean -> Tagged_GameState -> String    
placedDisksStatus bool taggedGameState = 
    if bool then
        "Placed disks: " <> placedDiskCountsStatus taggedGameState
    else
        ""
            
nameForStartRestartButton :: Boolean -> Players -> String
nameForStartRestartButton isGameStarted players =
    if isGameStarted then
        "Restart"
    else
        "Start" 
         