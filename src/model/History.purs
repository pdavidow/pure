module History
    ( History
    , makeHistory    
    , applyMoveOnHistory
    , swapLast
    , undoHistoryOnce
    )
    where

import Prelude

import Board (Move)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer, force)
import Data.List (drop, null, reverse, snoc)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Disk (Color(..), toggleColor)
import GameState (StartGameState(StartGameState), Tagged_GameState(Tagged_EndedGameState, Tagged_MidGameState, Tagged_StartGameState), applyMoveOnGameState, colorResultingInTaggedGameState, isForfeitTurn, mbNextMoveColor_FromTaggedGameState)
import MoveValidation (MoveValidationError, validateMove)
import Partial.Unsafe (unsafePartial)
import SequenceState (SequenceState, initialSequenceState)

type History = NE.NonEmptyList SequenceState    


makeHistory :: History
makeHistory =
    unsafePartial $ fromJust $ NE.fromFoldable [initialSequenceState]


applyMoveOnHistory :: Move -> History -> Either (NE.NonEmptyList MoveValidationError) History
applyMoveOnHistory move history =
    let
        state = NE.last history
        errors = validateMove move state.game
    in
        if null errors then
            Right $ NE.snoc history $ state {game = applyMoveOnGameState move state.game}
        else
            Left $ unsafePartial $ fromJust $ NE.fromList errors 


swapLast :: History -> SequenceState -> History
swapLast history x =
    unsafePartial fromJust $ NE.fromList $ snoc (NE.init history) x



undoHistoryOnce :: History -> Maybe History
undoHistoryOnce history = 
    undoHistoryOnceForColor color history
        where color = fromMaybe Black $ mbNextMoveColor_FromTaggedGameState $ (NE.last history).game-- should never use default


undoHistoryOnceForColor :: Color -> History -> Maybe History
undoHistoryOnceForColor color history = 
    let
        lastState = NE.last history
        toggledColor = toggleColor color
    in
        if NE.length history == 1 then 
            Nothing

        else if NE.length history == 2 then
            let 
                headState = NE.head history
            in
                case headState.game of
                    Tagged_StartGameState (StartGameState rec) -> -- always the case, by definition
                        if color == rec.color then
                            NE.fromFoldable [headState]
                        else
                            Nothing
    
                    Tagged_MidGameState _ -> 
                        Nothing -- should never get here

                    Tagged_EndedGameState _ -> 
                        Nothing -- should never get here

        else if isForfeitTurn lastState.game then
            NE.fromList $ NE.init history

        else
            let
                undoOnce :: Lazy (Maybe History)
                undoOnce = defer $ \ _ -> 
                    history
                        # NE.reverse
                        # NE.dropWhile (\ x -> colorResultingInTaggedGameState x.game == toggledColor)
                        # drop 1
                        # reverse
                        # NE.fromList
            in
                case lastState.game of
                    Tagged_StartGameState _ -> Nothing -- should never get here
                    Tagged_MidGameState _   -> force undoOnce
                    Tagged_EndedGameState _ -> force undoOnce                 