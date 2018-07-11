module GameHistory 
    ( MoveValidationError(..)
    , GameHistory
    , makeHistory
    , applyMoveOnHistory
    , undoHistoryOnce
    )
    where
      
import Prelude 
import GameState (StartGameState(..), Tagged_GameState(..), makeStartGameState, core_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState, nextMoves_FromTaggedGameState, applyMoveOnGameState, isZeroUnusedDiskCount, colorResultingInTaggedGameState, isForfeitTurn)
import Data.List.NonEmpty as NE
import Partial.Unsafe (unsafePartial)
import Board (Move(..)) 
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.List (elem, drop, fromFoldable, reverse)
import Disk (Color(..), toggleColor)
import Data.Either (Either(..))
import Data.Lazy (Lazy, defer, force)
import Data.Array (null)

data MoveValidationError
    = GameOver
    | WrongColor
    | NoAvailableDisk
    | NotOutflanking

type GameHistory = NE.NonEmptyList Tagged_GameState

derive instance eqMoveValidationError :: Eq MoveValidationError


makeHistory :: GameHistory
makeHistory =
    unsafePartial $ fromJust $ NE.fromFoldable [Tagged_StartGameState makeStartGameState]       


validateMove :: Move -> Tagged_GameState -> Array MoveValidationError 
validateMove (Move rec) taggedGameState = 
    let
        isGameOver = 
            case taggedGameState of
                Tagged_StartGameState _ -> false
                Tagged_MidGameState   _ -> false
                Tagged_EndedGameState _ -> true

        isWrongColor = 
            rec.color /= fromMaybe Black (mbNextMoveColor_FromTaggedGameState taggedGameState) -- should never use default

        isNoAvailableDisk = 
            isZeroUnusedDiskCount rec.color $ core_FromTaggedGameState taggedGameState

        isNotOutflanking =
            not $ elem rec.emptySquare $ map (\(Move rec') -> rec'.emptySquare) $ nextMoves_FromTaggedGameState taggedGameState
    in
        (if isGameOver        then [GameOver]        else []) <>
        (if isWrongColor      then [WrongColor]      else []) <>
        (if isNoAvailableDisk then [NoAvailableDisk] else []) <>
        (if isNotOutflanking  then [NotOutflanking]  else [])  


applyMoveOnHistory :: Move -> GameHistory -> Either (NE.NonEmptyList MoveValidationError) GameHistory
applyMoveOnHistory move history =
    let
        lastState = NE.last history
        errors = validateMove move lastState
        lzTaggedGameState = defer $ \ _ -> applyMoveOnGameState move lastState
    in
        if null errors then
            Right $ NE.snoc history $ force $ lzTaggedGameState
        else
            Left $ unsafePartial $ fromJust $ NE.fromList $ fromFoldable errors            


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