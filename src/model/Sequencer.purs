module Sequencer
    ( moveSequence
    , advanceHistory
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
import Data.Maybe (Maybe, fromJust, maybe)
import Debug.Trace (trace, traceAny)
import Disk (toggleColor)
import GameState (Tagged_GameState(..), mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState)
import History (History, applyMoveOnHistory, swapLast)
import Logger (logMoveErrors)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players, playerColored, setCurrentPlayerColorForSearch, isComputerVsComputer, isPersonVsPerson)
import Search (mbBestNextMove)
import Data.Tuple (Tuple(..))


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


moveSequence :: forall eff. History -> Eff (console :: CONSOLE, random :: RANDOM | eff) History
moveSequence history =
    moveSequence' count history 
        where 
            players = (NE.last history).players

            count = 
                if (isComputerVsComputer players) then 
                    1 
                else 
                    0    


moveSequence' :: forall eff. Int -> History -> Eff (console :: CONSOLE, random :: RANDOM | eff) History
moveSequence' count history = do
    if (traceAny (Tuple "count: " count) \_-> count) > 1 -- only done for the sake of ComputerVsComputer to user-step each move
        then do
            pure history 

        else do
            let state = NE.last history
            let (Player color playerType) = unsafe_CurrentPlayer state.players state.game

            case traceAny (Tuple "playerType: " playerType) \_-> playerType of
                Computer rec -> do
                    mbMove <-
                         if traceAny "rec.isRandomPick" \_-> rec.isRandomPick
                            then do 
                                let moves = nextMoves_FromTaggedGameState state.game
                                randN <- liftEff $ randomInt 0 $ length moves - 1
                                pure $ index moves randN 

                            else do
                                pure $ mbBestNextMove rec.searchDepth $ setCurrentPlayerColorForSearch state.game color 
                    
                    maybe 
                        (pure history) 
                        (\ move -> advanceHistory' count history move) 
                        mbMove

                Person rec -> do
                    if traceAny (Tuple "rec.isAutoSuggest: " rec.isAutoSuggest) \_-> rec.isAutoSuggest 
                        then do
                            let x = mbBestNextMove rec.searchDepth $ setCurrentPlayerColorForSearch state.game color
                            let state' = state {mbSuggestedMove = trace "x" \_-> x}
                            let history' = traceAny (Tuple "swapLast history state': " $ swapLast history state') \_-> swapLast history state'
                            pure history'
                            
                        else do
                            pure history


advanceHistory :: forall eff. History -> Move -> Eff (console :: CONSOLE, random :: RANDOM | eff) History
advanceHistory history move =
    -- only called for person move
    advanceHistory' count history move
        where 
            players = (NE.last history).players
            count = 
                if (isPersonVsPerson players) then 
                    traceAny "isPersonVsPerson players 0" \_-> 0
                else -- must be isPersonVsComputer
                    traceAny "isPersonVsComputer players -1" \_-> -1


advanceHistory' :: forall eff. Int -> History -> Move -> Eff (console :: CONSOLE, random :: RANDOM | eff) History
advanceHistory' count history move = do
    let eiHistory = traceAny "applyMoveOnHistory move history" \_-> applyMoveOnHistory move history
    let count' = count + 1

    if isRight eiHistory
        then do 
            let history' = unsafePartial fromRight eiHistory
            let taggedState = traceAny "(NE.last history').game" \_-> (NE.last history').game

            case taggedState of 
                Tagged_StartGameState _ -> moveSequence' count' history' -- should never get here
                Tagged_MidGameState   _ -> moveSequence' count' history'
                Tagged_EndedGameState _ -> pure history'
        else do -- should never get here (if coming from UI or gameTree-search)
            logMoveErrors move $ unsafePartial fromLeft eiHistory
            pure history
