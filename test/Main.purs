module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.List (List(..), (:), fromFoldable)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Position (PositionRow(..), makeValidPosition, radiatingPositionRows) 


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
      let p1 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
      let p2 = PositionRow (makeValidPosition {x:1, y:1} : Nil)
      let p3 = PositionRow (makeValidPosition {x:1, y:3} : Nil)
      let p4 = PositionRow (makeValidPosition {x:1, y:1} : makeValidPosition {x:1, y:1} : Nil)

      Assert.assert "" $ p1 == p2
      Assert.assert "" $ p1 /= p3
      Assert.assert "" $ p1 /= p4

      --Assert.equal 4 (2 + 2)
      --Assert.expectFailure "2 + 2 shouldn't be 5" $ Assert.equal 5 (2 + 2)
    test "radiatingPositionRows" do
      Assert.assert "{x:1, y:1}" $ (radiatingPositionRows (makeValidPosition {x:1, y:1})) == fromFoldable 
        [ PositionRow $ fromFoldable
            [ makeValidPosition {x:2, y:1}
            , makeValidPosition {x:3, y:1}
            , makeValidPosition {x:4, y:1}
            , makeValidPosition {x:5, y:1}
            , makeValidPosition {x:6, y:1}
            , makeValidPosition {x:7, y:1}
            , makeValidPosition {x:8, y:1}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:1, y:2}
            , makeValidPosition {x:1, y:3}
            , makeValidPosition {x:1, y:4}
            , makeValidPosition {x:1, y:5}
            , makeValidPosition {x:1, y:6}
            , makeValidPosition {x:1, y:7}
            , makeValidPosition {x:1, y:8}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:2, y:2}
            , makeValidPosition {x:3, y:3}
            , makeValidPosition {x:4, y:4}
            , makeValidPosition {x:5, y:5}
            , makeValidPosition {x:6, y:6}
            , makeValidPosition {x:7, y:7}
            , makeValidPosition {x:8, y:8}
            ]
        ]
      Assert.assert "{x:1, y:8}" $ (radiatingPositionRows (makeValidPosition {x:1, y:8}))  == fromFoldable 
        [ PositionRow $ fromFoldable
            [ makeValidPosition {x:2, y:8}
            , makeValidPosition {x:3, y:8}
            , makeValidPosition {x:4, y:8}
            , makeValidPosition {x:5, y:8}
            , makeValidPosition {x:6, y:8}
            , makeValidPosition {x:7, y:8}
            , makeValidPosition {x:8, y:8}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:1, y:7}
            , makeValidPosition {x:1, y:6}
            , makeValidPosition {x:1, y:5}
            , makeValidPosition {x:1, y:4}
            , makeValidPosition {x:1, y:3}
            , makeValidPosition {x:1, y:2}
            , makeValidPosition {x:1, y:1}            
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:2, y:7}
            , makeValidPosition {x:3, y:6}
            , makeValidPosition {x:4, y:5}
            , makeValidPosition {x:5, y:4}
            , makeValidPosition {x:6, y:3}
            , makeValidPosition {x:7, y:2}
            , makeValidPosition {x:8, y:1}
            ]
        ]        
      Assert.assert "{x:8, y:8}" $ (radiatingPositionRows (makeValidPosition {x:8, y:8}))  == fromFoldable 
        [ PositionRow $ fromFoldable
            [ makeValidPosition {x:7, y:8}
            , makeValidPosition {x:6, y:8}
            , makeValidPosition {x:5, y:8}
            , makeValidPosition {x:4, y:8}
            , makeValidPosition {x:3, y:8}
            , makeValidPosition {x:2, y:8}
            , makeValidPosition {x:1, y:8}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:8, y:7}
            , makeValidPosition {x:8, y:6}
            , makeValidPosition {x:8, y:5}
            , makeValidPosition {x:8, y:4}
            , makeValidPosition {x:8, y:3}
            , makeValidPosition {x:8, y:2}
            , makeValidPosition {x:8, y:1}            
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:7, y:7}
            , makeValidPosition {x:6, y:6}
            , makeValidPosition {x:5, y:5}
            , makeValidPosition {x:4, y:4}
            , makeValidPosition {x:3, y:3}
            , makeValidPosition {x:2, y:2}
            , makeValidPosition {x:1, y:1}
            ]
        ]          
      Assert.assert "{x:8, y:1}" $ (radiatingPositionRows (makeValidPosition {x:8, y:1}))  == fromFoldable 
        [ PositionRow $ fromFoldable
            [ makeValidPosition {x:7, y:1}
            , makeValidPosition {x:6, y:1}
            , makeValidPosition {x:5, y:1}
            , makeValidPosition {x:4, y:1}
            , makeValidPosition {x:3, y:1}
            , makeValidPosition {x:2, y:1}
            , makeValidPosition {x:1, y:1}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:8, y:2}
            , makeValidPosition {x:8, y:3}
            , makeValidPosition {x:8, y:4}
            , makeValidPosition {x:8, y:5}
            , makeValidPosition {x:8, y:6}
            , makeValidPosition {x:8, y:7}
            , makeValidPosition {x:8, y:8}            
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:7, y:2}
            , makeValidPosition {x:6, y:3}
            , makeValidPosition {x:5, y:4}
            , makeValidPosition {x:4, y:5}
            , makeValidPosition {x:3, y:6}
            , makeValidPosition {x:2, y:7}
            , makeValidPosition {x:1, y:8}
            ]
        ]          
      Assert.assert "{x:5, y:6}" $ (radiatingPositionRows (makeValidPosition {x:5, y:6}))  == fromFoldable 
        [ PositionRow $ fromFoldable
            [ makeValidPosition {x:4, y:6}
            , makeValidPosition {x:3, y:6}
            , makeValidPosition {x:2, y:6}
            , makeValidPosition {x:1, y:6}
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:6, y:6}
            , makeValidPosition {x:7, y:6}
            , makeValidPosition {x:8, y:6}
            ]      
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:5, y:7}
            , makeValidPosition {x:5, y:8}
            ]                     
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:5, y:5}
            , makeValidPosition {x:5, y:4}
            , makeValidPosition {x:5, y:3}
            , makeValidPosition {x:5, y:2}
            , makeValidPosition {x:5, y:1}           
            ]
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:4, y:7}
            , makeValidPosition {x:3, y:8}
            ]      
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:4, y:5}
            , makeValidPosition {x:3, y:4}
            , makeValidPosition {x:2, y:3}
            , makeValidPosition {x:1, y:2}            
            ]  
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:6, y:7}
            , makeValidPosition {x:7, y:8}
            ] 
        , PositionRow $ fromFoldable
            [ makeValidPosition {x:6, y:5}
            , makeValidPosition {x:7, y:4}
            , makeValidPosition {x:8, y:3}
            ]
        ]         