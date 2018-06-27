module SquareComponent where
--https://functionalprogramming.slack.com/files/U5KHK7Z1C/FAJ5VSRRQ/simplecomponent__with_aff_.hs

import Prelude

import Board (Tagged_Square(..), boardAt, initialBoard, isSquareColored)
import CSS (offset, toHexString)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (throwException)
import DOM (DOM)
import DOM.Classy.Element (id)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Position (makeValidPosition)
import Disk (Color(..))


data Query a
  = PreventDefault Event a
  | HandleInput Tagged_Square a

type Input = Tagged_Square
type State = Tagged_Square
type Effects eff = ( dom :: DOM | eff )

component :: forall eff. H.Component HH.HTML Query Input Void (Aff (Effects eff))
component =
    H.component
      { initialState: const (boardAt initialBoard $ makeValidPosition {x: 1, y: 1})
      , render
      , eval
      , receiver: HE.input HandleInput 
      }
    where


    render :: State -> H.ComponentHTML Query
    render state =
        HH.div
            [] -- HP.classes [ HH.ClassName "bg-light-yellow" ] ]
            [ HH.figure
                [ HP.classes [ HH.ClassName "h3 w3 bg-light-gray flex justify-center outline dim" ] --ma0 outline-0
                --, HE.onClick $ HE.input_ ToggleState
                , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                ]
                [ HH.div
                    [ HP.classes [ HH.ClassName classString ] ]
                    []
                ] 
            ]
            where
                classString = 
                    case state of 
                        Tagged_EmptySquare _ -> "br-100 h2 w2 bg-red self-center"
                        Tagged_FilledSquare x  -> 
                            if isSquareColored Black x then
                                "br-100 h2 w2 bg-black self-center"
                            else
                                "br-100 h2 w2 bg-white self-center"



    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of        
        HandleInput n next -> do
            oldN <- H.get
            when (oldN /= n) $ H.put n
            pure next

        PreventDefault event next -> do
            H.liftEff $ preventDefault event
            pure next