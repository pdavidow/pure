module History 
    ( MoveValidationError(..)
    , makeHistory
    , applyMoveOnHistory
    , undoHistoryOnce
    )
    where
      
import Prelude
import GameState (StartState(..), Tagged_State(..), makeStartState, core_FromTaggedState, mbNextMoveColor_FromTaggedState, nextMoves_FromTaggedState, applyMoveOnState, isZeroUnusedDiskCount, colorResultingInTaggedState, isForfeitTurn)
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


makeHistory :: NE.NonEmptyList Tagged_State
makeHistory =
    unsafePartial $ fromJust $ NE.fromFoldable [Tagged_StartState makeStartState]       


validateMoveOnHistory :: Move -> NE.NonEmptyList Tagged_State -> List MoveValidationError 
validateMoveOnHistory move history = 
    let
        lastState = NE.last history
        (Move color emptySquare _) = move

        isGameOver = 
            case lastState of
                Tagged_StartState _ -> false
                Tagged_MidState   _ -> false
                Tagged_EndState   _ -> true

        isWrongColor = 
            color /= fromMaybe Black (mbNextMoveColor_FromTaggedState lastState) -- should never use default

        isNoAvailableDisk = 
            isZeroUnusedDiskCount color $ core_FromTaggedState lastState

        isNotOutflanking =
            not $ elem emptySquare $ map (\(Move _ emptySquare' _) -> emptySquare') $ nextMoves_FromTaggedState lastState
    in
        fromFoldable $
            (if isGameOver        then [GameOver]        else []) <>
            (if isWrongColor      then [WrongColor]      else []) <>
            (if isNoAvailableDisk then [NoAvailableDisk] else []) <>
            (if isNotOutflanking  then [NotOutflanking]  else [])  


applyMoveOnHistory :: Move -> NE.NonEmptyList Tagged_State -> Either (NE.NonEmptyList MoveValidationError) (NE.NonEmptyList Tagged_State)
applyMoveOnHistory move history = 
    let
        errors = validateMoveOnHistory move history
        taggedState = applyMoveOnState move $ NE.last history
    in
        if null errors then
            Right $ NE.snoc history taggedState
        else
            Left $ unsafePartial $ fromJust $ NE.fromList $ errors            


undoHistoryOnce :: NE.NonEmptyList Tagged_State -> Maybe (NE.NonEmptyList Tagged_State)
undoHistoryOnce history = 
    undoHistoryOnceForColor color history
        where color = fromMaybe Black $ mbNextMoveColor_FromTaggedState $ NE.last history -- should never use default


undoHistoryOnceForColor :: Color -> NE.NonEmptyList Tagged_State -> Maybe (NE.NonEmptyList Tagged_State)
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
                case headState of
                    Tagged_StartState (StartState {color: color', nextMoves: _, core: _}) -> -- always the case, by definition
                        if color == color' then
                            NE.fromFoldable [headState]
                        else
                            Nothing
    
                    Tagged_MidState _ -> 
                        Nothing -- should never get here

                    Tagged_EndState _ -> 
                        Nothing -- should never get here

        else if isForfeitTurn lastState then
            NE.fromList $ NE.init history

        else
            case lastState of
                Tagged_StartState _ -> 
                    Nothing -- should never get here

                Tagged_MidState _ ->
                    history
                        # NE.reverse
                        # NE.dropWhile (\ x -> colorResultingInTaggedState x == toggledColor)
                        # drop 1
                        # reverse
                        # NE.fromList

                Tagged_EndState _ -> 
                    Nothing -- should never get here                    