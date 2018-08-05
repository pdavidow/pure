module Status
    ( status
    , treeSearchStatus
    , placedDisksStatus
    )
    where

import Prelude

import Board as B
import GameState as GS
import Player (Player(..), PlayerType(..), Players, searchDepth)
import Search (depthLevel)
import Sequencer (unsafe_CurrentPlayer, unsafe_OpponentPlayer) 


currentPlayer :: Players -> GS.Tagged_GameState -> Player
currentPlayer players taggedGameState = 
    unsafe_CurrentPlayer players taggedGameState 


opponentPlayer :: Players -> GS.Tagged_GameState -> Player
opponentPlayer players taggedGameState = 
    unsafe_OpponentPlayer players taggedGameState  


playerStatus :: Player -> String
playerStatus (Player color playerType) = 
    let
        suffix = show color
    in
        case playerType of
            Person   _ -> "Person " <> suffix
            Computer _ -> "Computer " <> suffix     


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
        midGameStatus_ForfeitTurn current opponent =
            midGameStatus current <> 
                " (" <> playerStatus opponent <> " forfeits turn)"


        midGameStatus_TransferDisk :: Player -> String
        midGameStatus_TransferDisk player =
            "TRANSFER UNUSED-DISK, " <> midGameStatus player                                                     
    in
        if isImminentGameStart || isGameStarted then
            case taggedGameState of
                GS.Tagged_StartGameState _ -> startGameStatus $ currentPlayer players taggedGameState

                GS.Tagged_MidGameState (GS.MidGameState rec) ->
                    case rec.status of
                        GS.Normal -> midGameStatus $ currentPlayer players taggedGameState                      
                        GS.ForfeitTurn_Rule2 ->  midGameStatus_ForfeitTurn (currentPlayer players taggedGameState) (opponentPlayer players taggedGameState)   
                        GS.TransferDisk_Rule9 -> midGameStatus_TransferDisk $ currentPlayer players taggedGameState

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


treeSearchStatus :: Players -> GS.Tagged_GameState -> String
treeSearchStatus players taggedGameState =
    let
        player = opponentPlayer players taggedGameState
        n = depthLevel $ searchDepth player
        what = if n == 1 then " level " else " levels " 
    in
        "Searching game-tree (" <> show n <> what <> "deep, for " <> playerStatus player <> ")..."
           