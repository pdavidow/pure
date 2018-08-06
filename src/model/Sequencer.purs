module Sequencer
    ( SequenceEffects
    , moveSequence
    , advanceHistoryFromPersonMove
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
import Disk (toggleColor)
import GameState (Tagged_GameState(..), mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState)
import History (History, applyMoveOnHistory, swapLast)
import Logger (logMoveErrors)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), PlayerType(..), Players, playerColored, setCurrentPlayerColorForSearch, isComputerVsComputer, isPersonVsPerson)
import Search (mbBestNextMove)
import SequenceState (SequenceState(..), seqRec)


type SequenceEffects eff = ( console :: CONSOLE, random :: RANDOM | eff )  


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


moveSequence :: forall eff. History -> Eff (SequenceEffects eff) History
moveSequence history =
    moveSequenceSeveral count history 
        where 
            players = (seqRec (NE.last history)).players

            count = 
                if (isComputerVsComputer players) then 
                    2
                else 
                    1    


moveSequenceSeveral :: forall eff. Int -> History -> Eff (SequenceEffects eff) History
moveSequenceSeveral count history = do
    if count > 2
        then do
            pure history 

        else do
            let state = NE.last history
            let srec = seqRec state
            let (Player color playerType) = unsafe_CurrentPlayer srec.players srec.game

            case playerType of 
                Computer rec -> do
                    mbMove <-
                         if rec.isRandomPick
                            then do 
                                let moves = nextMoves_FromTaggedGameState srec.game
                                randN <- liftEff $ randomInt 0 $ length moves - 1
                                pure $ index moves randN 

                            else do
                                pure $ mbBestNextMove rec.searchDepth $ setCurrentPlayerColorForSearch srec.game color 
                    
                    maybe 
                        (pure history) 
                        (\ move -> advanceHistory count history move) 
                        mbMove

                Person rec -> do
                    if rec.isAutoSuggest 
                        then do
                            let x = mbBestNextMove rec.searchDepth $ setCurrentPlayerColorForSearch srec.game color
                            let state' = SequenceState srec {mbSuggestedMove = x}
                            let history' = swapLast history state'
                            pure history'
                            
                        else do
                            pure history


advanceHistoryFromPersonMove :: forall eff. History -> Move -> Eff (SequenceEffects eff) History
advanceHistoryFromPersonMove history move =

    advanceHistory count history move
        where 
            players = (seqRec (NE.last history)).players
            count = 
                if (isPersonVsPerson players) then 
                    1
                else -- must be isPersonVsComputer 
                    0


advanceHistory :: forall eff. Int -> History -> Move -> Eff (SequenceEffects eff) History
advanceHistory count history move = do
    let eiHistory = applyMoveOnHistory move history
    let count' = count + 1

    if isRight eiHistory
        then do 
            let history' = unsafePartial fromRight eiHistory
            let taggedState = (seqRec (NE.last history')).game

            case taggedState of 
                Tagged_StartGameState _ -> moveSequenceSeveral count' history' -- should never get here
                Tagged_MidGameState   _ -> moveSequenceSeveral count' history'
                Tagged_EndedGameState _ -> pure history'
        else do -- should never get here (if coming from UI or gameTree-search)
            logMoveErrors move $ unsafePartial fromLeft eiHistory
            pure history
