module NavbarHTML
    ( navbar_HTML
    )
    where

import Prelude

import DOM.Classy.Event (toEvent)
import Display (nameForStartRestartButton)
import GameState (isStartGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Helper as HLPR
import Query (Query(..))
import State (State)
import Type.Data.Boolean (kind Boolean)


navbar_HTML :: State -> H.ComponentHTML Query 
navbar_HTML state =
    HH.span -- todo break out into Navbar_HTML module
        [ HP.classes [ HH.ClassName "ml3 roboto" ]
        ]  
        [ HH.span
            [ HP.classes [ HH.ClassName "b" ] -- todo "black bg_white hover_white hover_bg_black" effect doesn't work  
            ]    
            [ HH.text "OTHELLO" ]  
        , HH.button 
            [ HP.classes [ HH.ClassName "ml3 button is-small is-inverted is-outlined" ]
            , HP.disabled ( HLPR.isGameStarted state && (isStartGameState sequenceState.game) )
            , HE.onMouseEnter $ HE.input_ $ MouseEnter_StartStopButton
            , HE.onMouseLeave $ HE.input_ $ MouseLeave_StartStopButton                    
            , HE.onClick $ HE.input_ Click_GameStartRestart
            ] 
            [ HH.text $ nameForStartRestartButton (HLPR.isGameStarted state) sequenceState.players]
        , HH.a
            [ HP.classes [ HH.ClassName "ml3" ] -- button modal-button
            --, HP.prop (HH.PropName "data-target") CC.modalSettingsId 
            , HPA.hasPopup "true"
            , HE.onClick $ HE.input_ Click_Settings_Open
            ]
            [ HH.text "Settings" ]                      
        , HH.a
            [ HP.classes [ HH.ClassName "ml3" ]
            , HP.target "_blank" -- open in new tab
            , HP.href "http://www.boardgamecapital.com/game_rules/othello.pdf"
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]
            [ HH.text "Rules" ]
        , HH.a
            [ HP.classes [ HH.ClassName "ml3" ]
            , HP.target "_blank" -- open in new tab
            , HP.href "https://github.com/pdavidow/pure"
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]
            [ HH.text "GitHub" ]     
        ]    

        where 
            sequenceState = HLPR.sequenceStateOn state 