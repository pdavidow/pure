module Logger
    ( logMoveErrors
    )
    where
 
import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Board (Move(..))
import Data.List.NonEmpty as NE
import Control.Monad.Eff (Eff)
import Data.List (fold)
import MoveValidation (MoveValidationError)

logMoveErrors :: forall eff. Move -> NE.NonEmptyList MoveValidationError -> Eff (console :: CONSOLE | eff) Unit
logMoveErrors (Move rec) errors = do
    let uhohs = fold $ map (\x -> "\n" <> show x) $ NE.toList errors   

    log "\n================="
    log $ "INVALID MOVE: " <> show rec.emptySquare
    log uhohs
    log "\n================="

    pure unit    