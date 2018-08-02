module MoveValidation
    ( MoveValidationError(..)
    , validateMove
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


derive instance eqMoveValidationError :: Eq MoveValidationError

instance showMoveValidationError :: Show MoveValidationError where
    show error = 
        case error of
            WrongColor      -> "WrongColor"
            NoAvailableDisk -> "NoAvailableDisk"
            NotOutflanking  -> "NotOutflanking"    


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