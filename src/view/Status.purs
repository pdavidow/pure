module Status
    ( status
    , treeSearchStatus
    , placedDisksStatus
    )
    where

import Prelude

import Board as B
import ClassConstants as CC
import Data.Lazy (Lazy, defer, force)
import Data.List (List, concatMap, elem, filter, find, nub)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Disk (Color(..), toggleColor)
import GameState as GS
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players)
import Position (Position)
import SequenceState (SequenceStateRec)
import Sequencer (unsafe_CurrentPlayer, unsafe_OpponentPlayer)
import State (State)


status :: Boolean -> Boolean -> Players -> GS.Tagged_GameState -> String
status isImminentGameStart isGameStarted players taggedGameState =
    let
        startGameStatus :: Player -> String
        startGameStatus player = 
            playerStatus player <> " to move first"
                

        midGameStatus :: Player -> String
        midGameStatus player = 
            playerStatus player <> " to move next"


        midGameStatus_ForfeitTurn :: Player -> Player -> String
        midGameStatus_ForfeitTurn currentPlayer opponentPlayer =
            midGameStatus currentPlayer <> 
                " (" <> playerStatus opponentPlayer <> " forfeits turn)"


        midGameStatus_TransferDisk :: Player -> String
        midGameStatus_TransferDisk player =
            "TRANSFER UNUSED-DISK, " <> midGameStatus player


        playerStatus :: Player -> String
        playerStatus (Player color playerType) = 
            let
                suffix = show color
            in
                case playerType of
                    Person   _ -> "Person " <> suffix
                    Computer _ -> "Computer " <> suffix 


        lzCurrentPlayer :: Lazy Player
        lzCurrentPlayer = defer $ \_ -> unsafe_CurrentPlayer players taggedGameState 


        lzOpponentPlayer :: Lazy Player
        lzOpponentPlayer = defer $ \_ -> unsafe_OpponentPlayer players taggedGameState                                                        
    in
        if isImminentGameStart || isGameStarted then
            case taggedGameState of
                GS.Tagged_StartGameState _ -> startGameStatus $ force lzCurrentPlayer

                GS.Tagged_MidGameState (GS.MidGameState rec) ->
                    case rec.status of
                        GS.Normal -> midGameStatus $ force lzCurrentPlayer                      
                        GS.ForfeitTurn_Rule2 ->  midGameStatus_ForfeitTurn (force lzCurrentPlayer) (force lzOpponentPlayer)   
                        GS.TransferDisk_Rule9 -> midGameStatus_TransferDisk $ force lzCurrentPlayer

                GS.Tagged_EndedGameState x -> gameSummaryDisplay x
        else
            "" 


gameSummaryDisplay :: GS.EndedGameState -> String
gameSummaryDisplay x@(GS.EndedGameState rec) =
    let 
        reasonString = case rec.status of
            GS.NoUnusedDisksForBoth -> "No more available disks for either player"
            GS.NoValidMoves         -> "No more valid moves"

        winnerString = case GS.winner x of
            GS.WinColor color -> "Winner is " <> (show color)
            GS.Tie            -> "TIE game"
    in 
        "GAME OVER (" <> reasonString <> ") " <> winnerString        


placedDisksStatus :: Boolean -> GS.Tagged_GameState -> String    
placedDisksStatus bool taggedGameState = 
    if bool then
        "Placed disks: " <> placedDiskCountsStatus taggedGameState
    else
        ""        


placedDiskCountsStatus :: GS.Tagged_GameState -> String
placedDiskCountsStatus taggedGameState =
    GS.board_FromTaggedGameState taggedGameState
        # B.squaresColoredCounts_BlackWhite
        # show      


treeSearchStatus :: String
treeSearchStatus =
    "Searching game-tree..."
          