module DashboardFooterHTML
    ( dashboardFooter_HTML
    )

    where

import Prelude

import ClassConstants as CC
import Data.Monoid (guard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Query (Query)
import State (State)
import Status (status, treeSearchStatus) 
import Type.Data.Boolean (kind Boolean)


dashboardFooter_HTML :: State -> H.ComponentHTML Query  
dashboardFooter_HTML state = 
    HH.div
        [ HP.classes [ HH.ClassName "mt2 f3 lh-copy b" ]
        ] $
        [ HH.text $ status state.isImminentGameStart (HLPR.isGameStarted state) srec.players srec.game 
        ]  
        <> guard state.isBlockingOnSearch
            [ HH.div_
                [ HH.label
                    [ HP.classes [ HH.ClassName CC.treeSearchStatus ] 
                    ]
                    [ HH.text $ treeSearchStatus srec.players srec.game ]  
                ]
            ]             

    where
        srec = HLPR.sequenceStateRecOn state   