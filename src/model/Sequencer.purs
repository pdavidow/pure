module Sequencer
    ( moveSequence
    , advanceHistory
    , mbSuggestedMove
    , mbCurrentPlayer
    , unsafe_CurrentPlayer
    , mbOpponentPlayer
    , unsafe_OpponentPlayer
    )
    where 

import Prelude

import Board (Move)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import Data.Either (isRight, fromLeft, fromRight)
import Data.List (index, length)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, maybe)
import GameHistory (GameHistory, applyMoveOnHistory)
import GameState (Tagged_GameState(..), mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players, playerColored, setCurrentPlayerColorForSearch, isComputerVsComputer) 
import Search (mbBestNextMove)
import Logger (logMoveErrors)  
import Disk (toggleColor)


mbCurrentPlayer :: Players -> Tagged_GameState -> Maybe Player
mbCurrentPlayer players t =
    playerColored players 
        <$> 
            mbNextMoveColor_FromTaggedGameState t


unsafe_CurrentPlayer :: Players -> Tagged_GameState -> Player
unsafe_CurrentPlayer players t =
    unsafePartial fromJust $ mbCurrentPlayer players t


mbOpponentPlayer :: Players -> Tagged_GameState -> Maybe Player
mbOpponentPlayer players t =
    (\c -> playerColored players $ toggleColor c) 
        <$> 
            mbNextMoveColor_FromTaggedGameState t


unsafe_OpponentPlayer :: Players -> Tagged_GameState -> Player
unsafe_OpponentPlayer players t =
    unsafePartial fromJust $ mbOpponentPlayer players t


moveSequence :: forall eff. Players -> GameHistory -> Eff (console :: CONSOLE, random :: RANDOM | eff) GameHistory
moveSequence players history =
    moveSequence' count players history 
        where count = if (isComputerVsComputer players) then 1 else 0       


moveSequence' :: forall eff. Int -> Players -> GameHistory -> Eff (console :: CONSOLE, random :: RANDOM | eff) GameHistory
moveSequence' count players history = do
    if count > 1 -- only done for the sake of ComputerVsComputer to break out after each move
        then do
            pure history 

        else do
            let taggedGameState = NE.last history
            let (Player color playerType) = unsafe_CurrentPlayer players taggedGameState 

            mbMove <- case playerType of
                Person _ -> do
                    pure Nothing

                Computer rec -> do
                    case rec.isRandomPick of    
                        true -> do 
                            let moves = nextMoves_FromTaggedGameState taggedGameState
                            randN <- liftEff $ randomInt 0 $ length moves - 1 -- https://purescript-users.ml/t/problem-with-using-rand-in-a-let/292/2
                            pure $ index moves randN 

                        false -> do
                            let taggedGameState' = setCurrentPlayerColorForSearch taggedGameState color
                            pure $ mbBestNextMove rec.searchDepth taggedGameState' 

            maybe (pure history) (\ move -> advanceHistory' count players history move) mbMove


advanceHistory :: forall eff. Players -> GameHistory -> Move -> Eff (console :: CONSOLE, random :: RANDOM | eff) GameHistory
advanceHistory players history move =
    advanceHistory' 0 players history move


advanceHistory' :: forall eff. Int -> Players -> GameHistory -> Move -> Eff (console :: CONSOLE, random :: RANDOM | eff) GameHistory
advanceHistory' count players history move = do
    let eiHistory = applyMoveOnHistory move history
    let count' = count + 1

    if isRight eiHistory -- https://purescript-users.ml/t/when-do-vs-if-then-do-else/249/3
        then do 
            let history' = unsafePartial fromRight eiHistory
            let taggedState = NE.last history'

            case taggedState of 
                Tagged_StartGameState _ -> moveSequence' count' players history' -- should never get here
                Tagged_MidGameState   _ -> moveSequence' count' players history'
                Tagged_EndedGameState _ -> pure history'
        else do -- should never get here (if coming from UI or gameTree-search)
            logMoveErrors move $ unsafePartial fromLeft eiHistory
            pure history
     

mbSuggestedMove :: Players -> GameHistory -> Maybe Move
mbSuggestedMove players history =
    let
        taggedGameState = NE.last history
        mbPlayer = mbCurrentPlayer players taggedGameState
    in
        case mbPlayer of
            Just (Player color playerType) ->
                case playerType of
                    Person rec ->
                        if rec.isAutoSuggest then
                            mbBestNextMove rec.searchDepth $ setCurrentPlayerColorForSearch taggedGameState color
                        else
                            Nothing

                    Computer _ -> 
                        Nothing    

            Nothing -> 
                Nothing
