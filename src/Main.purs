module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA   
import Halogen.VDom.Driver (runUI)   
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import BoardComponent (component) 
 

main :: Eff (HA.HalogenEffects (console :: CONSOLE, random :: RANDOM)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body