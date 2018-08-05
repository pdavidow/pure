module DashboardHTML
    ( dashboard_HTML
    )

    where

import Prelude 

import Data.Monoid (guard)
import Display (gameOver_Emphasis)
import GameState (isEndedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Player (isComputerVsComputer)
import Query (Query(..))
import State (State)
import Status (placedDisksStatus)
import Type.Data.Boolean (kind Boolean)


dashboard_HTML :: State -> H.ComponentHTML Query  
dashboard_HTML state =
    HH.span 
        [ HP.classes [ HH.ClassName "mt2" ] -- todo unused controls-grid"
        ] $
        [ HH.button
            [ HP.classes [ HH.ClassName "" ]
            , HP.enabled $ HLPR.isHistoryUndoable state.history
            , HE.onClick $ HE.input_ Undo
            ]
            [ HH.text "Undo" ] 
        , HH.button
            [ HP.classes [ HH.ClassName "ml4" ]
            , HE.onClick $ HE.input_ Click_FlipCounts
            , HP.disabled $ not $ HLPR.isGameStarted state
            ]
            [ HH.text "Flip Counts" ]    
        ]
        <> guard (isComputerVsComputer rec.players) 
        [ HH.button
            [ HP.classes [ HH.ClassName "ml4" ]
            , HE.onClick $ HE.input_ Click_ComputerStep
            , HP.disabled $ isDisabled_ComputerStep
            ]
            [ HH.text "Computer Step" ]  
        ]  
        <>
        [ HH.span
            [ HP.classes [ HH.ClassName $ "ml4 " <> gameOver_Emphasis rec.game ] 
            ]
            [ HH.text $ placedDisksStatus (HLPR.isGameStarted state) rec.game]     
        ]  

        where
            rec = HLPR.sequenceStateRecOn state 

            isDisabled_ComputerStep = 
                (not $ HLPR.isGameStarted state) 
                || 
                isEndedGameState rec.game 