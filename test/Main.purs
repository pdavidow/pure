module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.List (List(..), (:))
import Position (PositionRow(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall t1.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | t1
    )
    Unit
main = runTest do
  suite "Position" do
    test "eqPositionRow" do
      let p1 = PositionRow ({x:1, y:1} : Nil)
      let p2 = PositionRow ({x:1, y:1} : Nil)
      let p3 = PositionRow ({x:1, y:3} : Nil)
      let p4 = PositionRow ({x:1, y:1} : {x:1, y:1} : Nil)

      Assert.assert "" $ p1 == p2
      Assert.assert "" $ p1 /= p3
      Assert.assert "" $ p1 /= p4

      --Assert.equal 4 (2 + 2)
      --Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)