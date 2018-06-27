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
import Data.List (List, elem, drop, fromFoldable, null, reverse)
import Disk (Color(..), toggleColor)
import Data.Either (Either(..))


data MoveValidationError
    = GameOver
    | WrongColor
    | NoAvailableDisk
    | NotOutflanking
    | DefaultDummy  

type GameHistory = NE.NonEmptyList Tagged_GameState

derive instance eqMoveValidationError :: Eq MoveValidationError


makeHistory :: GameHistory
makeHistory =
    unsafePartial $ fromJust $ NE.fromFoldable [Tagged_StartGameState makeStartGameState]       


validateMoveOnHistory :: Move -> GameHistory -> List MoveValidationError 
validateMoveOnHistory move history = 
    let
        lastGameState = NE.last history
        (Move color emptySquare _) = move

        isGameOver = 
            case lastGameState of
                Tagged_StartGameState _ -> false
                Tagged_MidGameState   _ -> false
                Tagged_EndGameState   _ -> true

        isWrongColor = 
            color /= fromMaybe Black (mbNextMoveColor_FromTaggedGameState lastGameState) -- should never use default

        isNoAvailableDisk = 
            isZeroUnusedDiskCount color $ core_FromTaggedGameState lastGameState

        isNotOutflanking =
            not $ elem emptySquare $ map (\(Move _ emptySquare' _) -> emptySquare') $ nextMoves_FromTaggedGameState lastGameState
    in
        fromFoldable $
            (if isGameOver        then [GameOver]        else []) <>
            (if isWrongColor      then [WrongColor]      else []) <>
            (if isNoAvailableDisk then [NoAvailableDisk] else []) <>
            (if isNotOutflanking  then [NotOutflanking]  else [])  


applyMoveOnHistory :: Move -> GameHistory -> Either (NE.NonEmptyList MoveValidationError) GameHistory
applyMoveOnHistory move history =
    let
        errors = validateMoveOnHistory move history
        taggedGameState = applyMoveOnGameState move $ NE.last history
    in
        if null errors then
            Right $ NE.snoc history taggedGameState
        else
            Left $ unsafePartial $ fromJust $ NE.fromList $ errors            


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
                    Tagged_StartGameState (StartGameState {color: color', nextMoves: _, core: _}) -> -- always the case, by definition
                        if color == color' then
                            NE.fromFoldable [headGameState]
                        else
                            Nothing
    
                    Tagged_MidGameState _ -> 
                        Nothing -- should never get here

                    Tagged_EndGameState _ -> 
                        Nothing -- should never get here

        else if isForfeitTurn lastGameState then
            NE.fromList $ NE.init history

        else
            case lastGameState of
                Tagged_StartGameState _ -> 
                    Nothing -- should never get here

                Tagged_MidGameState _ ->
                    history
                        # NE.reverse
                        # NE.dropWhile (\ x -> colorResultingInTaggedGameState x == toggledColor)
                        # drop 1
                        # reverse
                        # NE.fromList

                Tagged_EndGameState _ -> 
                    Nothing -- should never get here                    