module GameHistory 
    ( MoveValidationError(..)
    , GameHistory
    , makeHistory
    , applyMoveOnHistory
    , undoHistoryOnce
    )
    where
      
import Prelude

import Board (Move(..))
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer, force)
import Data.List (List, elem, drop, fromFoldable, null, reverse)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Disk (Color(..), toggleColor)
import GameState (StartGameState(..), Tagged_GameState(..), makeStartGameState, mbNextMoveColor_FromTaggedGameState, core_FromTaggedGameState, nextMoveColor_FromStartGameState, nextMoveColor_FromMidGameState, nextMoves_FromTaggedGameState, applyMoveOnGameState, isZeroUnusedDiskCount, colorResultingInTaggedGameState, isForfeitTurn)
import Lib (lefts)
import Partial.Unsafe (unsafePartial)


data MoveValidationError
    = WrongColor
    | NoAvailableDisk
    | NotOutflanking

type GameHistory = NE.NonEmptyList Tagged_GameState

derive instance eqMoveValidationError :: Eq MoveValidationError

instance showMoveValidationError :: Show MoveValidationError where
    show error = 
        case error of
            WrongColor      -> "WrongColor"
            NoAvailableDisk -> "NoAvailableDisk"
            NotOutflanking  -> "NotOutflanking"


makeHistory :: GameHistory
makeHistory =
    unsafePartial $ fromJust $ NE.fromFoldable [Tagged_StartGameState makeStartGameState]       


eiWrongColor :: Move -> Tagged_GameState -> Either MoveValidationError Unit 
eiWrongColor (Move rec) taggedGameState = 
    let
        f = \ color -> if rec.color == color then Right unit else Left WrongColor
    in
        case taggedGameState of
            Tagged_StartGameState x -> f $ nextMoveColor_FromStartGameState x
            Tagged_MidGameState   x -> f $ nextMoveColor_FromMidGameState x
            Tagged_EndedGameState _ -> Left WrongColor -- should never get here


eiNoAvailableDisk :: Move -> Tagged_GameState -> Either MoveValidationError Unit 
eiNoAvailableDisk (Move rec) taggedGameState = 
    if isZero then Left NoAvailableDisk else Right unit
    where isZero = isZeroUnusedDiskCount rec.color $ core_FromTaggedGameState taggedGameState


eiNotOutflanking :: Move -> Tagged_GameState -> Either MoveValidationError Unit 
eiNotOutflanking (Move rec) taggedGameState =
    if isOutflanking then Right unit else Left NotOutflanking
    where isOutflanking = elem rec.emptySquare $ map (\(Move rec') -> rec'.emptySquare) $ nextMoves_FromTaggedGameState taggedGameState


validateMove :: Move -> Tagged_GameState -> List MoveValidationError 
validateMove move taggedGameState = 
    [ eiWrongColor
    , eiNoAvailableDisk
    , eiNotOutflanking
    ]
        # map (\f -> f move taggedGameState)
        # fromFoldable
        # lefts


applyMoveOnHistory :: Move -> GameHistory -> Either (NE.NonEmptyList MoveValidationError) GameHistory
applyMoveOnHistory move history =
    let
        lastState = NE.last history
        errors = validateMove move lastState
    in
        if null errors then
            Right $ NE.snoc history $ applyMoveOnGameState move lastState
        else
            Left $ unsafePartial $ fromJust $ NE.fromList errors            


undoHistoryOnce :: GameHistory -> Maybe GameHistory
undoHistoryOnce history = 
    undoHistoryOnceForColor color history
        where color = fromMaybe Black $ mbNextMoveColor_FromTaggedGameState $ NE.last history -- should never use default


undoHistoryOnceForColor :: Color -> GameHistory -> Maybe GameHistory
undoHistoryOnceForColor color history = 
    let
        lastGameState = NE.last history
        toggledColor = toggleColor color
    in
        if NE.length history == 1 then 
            Nothing

        else if NE.length history == 2 then
            let 
                headGameState = NE.head history
            in
                case headGameState of
                    Tagged_StartGameState (StartGameState rec) -> -- always the case, by definition
                        if color == rec.color then
                            NE.fromFoldable [headGameState]
                        else
                            Nothing
    
                    Tagged_MidGameState _ -> 
                        Nothing -- should never get here

                    Tagged_EndedGameState _ -> 
                        Nothing -- should never get here

        else if isForfeitTurn lastGameState then
            NE.fromList $ NE.init history

        else
            let
                undoOnce :: Lazy (Maybe GameHistory)
                undoOnce = defer $ \ _ -> 
                    history
                        # NE.reverse
                        # NE.dropWhile (\ x -> colorResultingInTaggedGameState x == toggledColor)
                        # drop 1
                        # reverse
                        # NE.fromList
            in
                case lastGameState of
                    Tagged_StartGameState _ -> Nothing -- should never get here
                    Tagged_MidGameState _   -> force undoOnce
                    Tagged_EndedGameState _ -> force undoOnce                  