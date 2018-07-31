module DashboardHTML
    ( dashboard_HTML
    )

    where

import Prelude

import Data.Monoid (guard)
import Display (gameOver_Emphasis, placedDisksStatus)
import GameState (isEndedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Player (isComputerVsComputer)
import Query (Query(..))
import State (State)
import Type.Data.Boolean (kind Boolean)


dashboard_HTML :: State -> H.ComponentHTML Query 
dashboard_HTML state =
    HH.span 
        [ HP.classes [ HH.ClassName "mt2" ] -- todo unused controls-grid"
        ] $
        [ HH.button
            [ HP.classes [ HH.ClassName "" ]
            , HP.enabled $ HLPR.isHistoryUndoable state.gameHistory
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
        <> guard (isComputerVsComputer state.players) 
        [ HH.button
            [ HP.classes [ HH.ClassName "ml4" ]
            , HE.onClick $ HE.input_ Click_ComputerStep
            , HP.disabled $ isDisabled_ComputerStep state
            ]
            [ HH.text "Computer Step" ]  
        ]  
        <>
        [ HH.span
            [ HP.classes [ HH.ClassName $ "ml4 " <> (gameOver_Emphasis $ HLPR.gameState state) ] 
            ]
            [ HH.text $ placedDisksStatus (HLPR.isGameStarted state) $ HLPR.gameState state]     
        ] 


isDisabled_ComputerStep :: State -> Boolean
isDisabled_ComputerStep state =  
    ( not $ HLPR.isGameStarted state )     
        || 
            ( isEndedGameState $ HLPR.gameState state )