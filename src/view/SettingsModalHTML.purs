module SettingsModalHTML
    ( settingsModal_HTML )
    where
      
import Prelude

import BlackWhite (getItemColored)
import DOM.HTML.Indexed.InputType as DOMT
import Disk (Color(..))
import Display (isActiveClass_Tag)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Player (Player, isPlayer_Computer, isPlayer_Person)
import Query (Query(..))
import State (State)
import Type.Data.Boolean (kind Boolean)


settingsModal_HTML :: State -> H.ComponentHTML Query 
settingsModal_HTML state =
    HH.div 
        [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass_Tag state.isActive_SettingsModal)  ]
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "modal-background" ]
            ]
            []
        , HH.div
            [ HP.classes [ HH.ClassName "modal-card" ]
            ]
            [ HH.header
                [ HP.classes [ HH.ClassName "modal-card-head" ]
                ]
                [ HH.p
                    [ HP.classes [ HH.ClassName "modal-card-title" ]
                    ]
                    [ HH.text "Settings"]
                , HH.button
                    [ HP.classes [ HH.ClassName "delete" ]
                    , HPA.label "close"
                    , HE.onClick $ HE.input_ Click_Close_Settings
                    ]
                    []
                ]
            , HH.section
                [ HP.classes [ HH.ClassName "modal-card-body" ]
                ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "tabs" ]                            
                    ]
                    [ HH.ul_
                        [ HH.li
                            [ HP.classes [ HH.ClassName $ isActiveClass_Tag $ state.activeSettingsColor == Black ]                            
                            ]
                            [ HH.a
                                [ HE.onClick $ HE.input_ $ Click_Settings Black ]
                                [ HH.text "Black" ]
                            ]
                        , HH.li
                            [ HP.classes [ HH.ClassName $ isActiveClass_Tag $ state.activeSettingsColor == White ]                            
                            ]
                            [ HH.a
                                [ HE.onClick $ HE.input_ $ Click_Settings White ]
                                [ HH.text "White" ]
                            ]                                    
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control" ]                            
                    ]
                    [ HH.label
                        [ HP.classes [ HH.ClassName "radio" ]                            
                        ]
                        [ HH.span_
                            [ HH.input 
                                [ HP.type_ DOMT.InputRadio
                                , HP.name "player-type" 
                                , HP.checked $ isPlayer_Person playerForActiveSetting
                                , HE.onClick $ HE.input_ $ Click_Settings_Person state.activeSettingsColor
                                ]                               
                            , HH.text "Person" 
                            ] 
                        ]   
                    , HH.label
                        [ HP.classes [ HH.ClassName "radio" ]                            
                        ]
                        [ HH.span_
                            [ HH.input 
                                [ HP.type_ DOMT.InputRadio
                                , HP.name "player-type" 
                                , HP.checked $ isPlayer_Computer playerForActiveSetting
                                , HE.onClick $ HE.input_ $ Click_Settings_Computer state.activeSettingsColor
                                ]                               
                            , HH.text "Computer" 
                            ] 
                        ]                                                                
                    ]
                ]
            , HH.footer 
                [ HP.classes [ HH.ClassName "modal-card-foot" ]
                ]
                [ HH.button
                    [ HP.classes [ HH.ClassName "button is-warning" ]
                    , HE.onClick (HE.input_ Click_ResetSettingsToDefaults)
                    ]
                    [ HH.text "Reset to Defaults" ]  
                ]
            ]
        ]

    where
          
    playerForActiveSetting :: Player
    playerForActiveSetting =
        getItemColored (state.activeSettingsColor) state.players