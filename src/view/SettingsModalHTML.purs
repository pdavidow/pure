module SettingsModalHTML
    ( settingsModal_HTML
    )
     where      

import Prelude

import BlackWhite (getItemColored)
import ClassConstants as CC
import DOM.HTML.Indexed.InputType as DOMT
import Data.Array (length, range, zipWith)
import Data.Monoid (guard)
import Disk (Color(..))
import Display (isActiveClass_Tag, isInvisibleClass_Tag)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Helper as HLPR
import PlayerDefaults as DFLT
import Query (Query(..))
import Search (SearchDepth, searchDepths)
import Settings (EditPlayer(..), EditPlayers, EditPlayerType(..), EditPlayerTypeRec, toPlayers)
import State (State)
import Type.Data.Boolean (kind Boolean)


settingsModal_HTML :: State -> H.ComponentHTML Query 
settingsModal_HTML state =
    HH.div 
        [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass_Tag state.isShowModal_Settings)  ]
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "modal-background" ]
            ]
            []
        , HH.div
            [ HP.classes [ HH.ClassName "modal-card" ]
            ]
            [ head_HTML
            , body_HTML
            , foot_HTML
            ]
        ]

    where

    head_HTML :: H.ComponentHTML Query
    head_HTML =
        HH.header
            [ HP.classes [ HH.ClassName "modal-card-head" ]
            ]
            [ HH.p
                [ HP.classes [ HH.ClassName "modal-card-title" ]
                ]
                [ HH.text "Settings"]
            , HH.button
                [ HP.classes [ HH.ClassName "delete" ]
                , HPA.label "close"
                , HE.onClick $ HE.input_ $ Click_Settings_Cancel isPendingChanges
                ]
                []
            ]
          

    body_HTML :: H.ComponentHTML Query
    body_HTML =
        HH.section
            [ HP.classes [ HH.ClassName "modal-card-body" ]
            ] 
            [ body_Tabs_HTML
            , body_PlayerType_HTML 
            , HH.section 
                [ HP.classes [ HH.ClassName "ml3 mt3" ]                            
                ] $ []
                <> guard isSelected_Computer 
                [ body_ComputerDetails_HTML                                                                                                                                                                        
                ]
                <> guard isSelected_Person 
                [ body_PersonDetails_HTML                                                                                                                                                                        
                ]
            ]


    body_Tabs_HTML :: H.ComponentHTML Query 
    body_Tabs_HTML =
        HH.div
            [ HP.classes [ HH.ClassName "tabs is-boxed" ]                            
            ]
            [ HH.ul_
                [ HH.li
                    [ HP.classes [ HH.ClassName $ "b " <> (isActiveClass_Tag $ selectedColor == Black) ]                            
                    ]
                    [ HH.a
                        [ HE.onClick $ HE.input_ $ Click_Settings_selectedColor Black ] 
                        [ HH.text "Black" ]
                    ]
                , HH.li
                    [ HP.classes [ HH.ClassName $ "b " <> (isActiveClass_Tag $ selectedColor == White) ]                            
                    ]
                    [ HH.a
                        [ HE.onClick $ HE.input_ $ Click_Settings_selectedColor White ]
                        [ HH.text "White" ]
                    ]                                    
                ]
            ]


    body_PlayerType_HTML :: H.ComponentHTML Query
    body_PlayerType_HTML =
        HH.div_
            [ HH.div 
                [ HP.classes [ HH.ClassName "b" ]
                ]
                [ HH.text "Player Type" ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control" ]                                               
                    ]
                    [ HH.label
                        [ HP.classes [ HH.ClassName "radio" ]                           
                        ]
                        [ HH.span
                            [ HP.classes [ HH.ClassName "b" ]
                            ]                        
                            [ HH.input 
                                [ HP.type_ DOMT.InputRadio
                                , HP.name "PlayerType"
                                , HP.checked isSelected_Computer
                                , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ \ r -> r {playerType = EditComputer} 
                                --, HP.disabled isDisabled_PlayerType 
                                ]                               
                            , HH.text "Computer" 
                            ] 
                        ]                       
                    , HH.label
                        [ HP.classes [ HH.ClassName "radio" ]                            
                        ]
                        [ HH.span
                            [ HP.classes [ HH.ClassName "b" ]
                            ] 
                            [ HH.input 
                                [ HP.type_ DOMT.InputRadio
                                , HP.name "PlayerType" 
                                , HP.checked isSelected_Person
                                , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ \ r -> r {playerType = EditPerson} 
                                --, HP.disabled isDisabled_PlayerType
                                ]                               
                            , HH.text "Person" 
                            ] 
                        ] 
                    ]
                ]            


    body_ComputerDetails_HTML :: H.ComponentHTML Query
    body_ComputerDetails_HTML =
        HH.section_
            [ HH.div_ 
                [ HH.text "Strategy" ]        
            , HH.div
                [ HP.classes [ HH.ClassName "control" ]                                               
                ]
                [ HH.label
                    [ HP.classes [ HH.ClassName "radio" ]                            
                    ]
                    [ HH.span_
                        [ HH.input 
                            [ HP.type_ DOMT.InputRadio
                            , HP.name "ComputerStrategy"  
                            , HP.checked $ editRec.computer_isRandomPick
                            , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ \ r -> r {computer_isRandomPick = true}  
                            ]                               
                        , HH.text "Random"  
                        ] 
                    ]   
                , HH.label
                    [ HP.classes [ HH.ClassName "radio" ]                           
                    ]
                    [ HH.span_
                        [ HH.input 
                            [ HP.type_ DOMT.InputRadio
                            , HP.name "ComputerStrategy"  
                            , HP.checked isSelected_ComputerDetails_SearchDepth
                            , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ \ r -> r {computer_isRandomPick = false} 
                            ]                               
                        , HH.text "Search" 
                        ]  
                    ]                     
                ]   
            , HH.section
                [ HP.classes [ HH.ClassName $ isInvisibleClass_Tag $ not isSelected_ComputerDetails_SearchDepth ]                            
                ] 
                [ HH.div
                    [ HP.classes [ HH.ClassName "mt2" ]                         
                    ]            
                    [ HH.div_ 
                        [ HH.text "Depth" ]  
                    , searchOptions -- todo refactor
                        "ComputerSearchDepth"
                        (\ depth -> editRec.computer_searchDepth == depth)
                        (\ depth r -> r {computer_searchDepth = depth}) 
                    ]
                ]   
            ] 


    body_PersonDetails_HTML :: H.ComponentHTML Query
    body_PersonDetails_HTML = 
        HH.section
            [ HP.classes [ HH.ClassName body_PersonDetails_section_classes ]                         
            ]         
            [ HH.label
                [ HP.classes [ HH.ClassName "checkbox" ]                            
                ]
                [ HH.span_
                    [ HH.input 
                        [ HP.type_ DOMT.InputCheckbox
                        , HP.checked $ editRec.person_isAutoSuggest
                        , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ \ r -> r {person_isAutoSuggest = not editRec.person_isAutoSuggest}  
                        ]                               
                    , HH.text "Auto Suggest"  
                    ] 
                ]   
            , HH.section
                [ HP.classes [ HH.ClassName $ isInvisibleClass_Tag $ not isSelected_PersonDetails_SearchDepth ]                            
                ] 
                [ HH.div
                    [ HP.classes [ HH.ClassName "mt2" ]                         
                    ]            
                    [ HH.div_ 
                        [ HH.text "Search Depth" ]  
                    , searchOptions -- todo refactor
                        "PersonSearchDepth"
                        (\ depth -> editRec.person_searchDepth == depth)
                        (\ depth r -> r {person_searchDepth = depth}) 
                    ]
                ]                 
            ]
     

    searchOptions :: String -> (SearchDepth -> Boolean) -> (SearchDepth -> EditPlayerTypeRec -> EditPlayerTypeRec) -> H.ComponentHTML Query
    searchOptions widgetSetName isChecked modifier =
        HH.div    
            [ HP.classes [ HH.ClassName "control" ]                                               
            ]  
            xs

        where

        xs :: Array (H.ComponentHTML Query)
        xs = zipWith 
            (f widgetSetName isChecked modifier) 
            searchDepths 
            (range (1 :: Int) $ length searchDepths)


        f :: String -> (SearchDepth -> Boolean) -> (SearchDepth -> EditPlayerTypeRec -> EditPlayerTypeRec) -> SearchDepth -> Int -> H.ComponentHTML Query
        f widgetSetName' isChecked' modifier' depth n = -- prime notation is to get rid of shadowing warnings
            HH.label 
                [ HP.classes [ HH.ClassName "radio" ]                            
                ]
                [ HH.span_
                    [ HH.input 
                        [ HP.type_ DOMT.InputRadio
                        , HP.name widgetSetName'   
                        , HP.checked $ isChecked' depth 
                        , HE.onClick $ HE.input_ $ ModifySettings selectedColor $ modifier' depth  
                        ]                                
                    , HH.text $ show n 
                    ] 
                ]   


    foot_HTML :: H.ComponentHTML Query
    foot_HTML =
        HH.footer 
            [ HP.classes [ HH.ClassName "modal-card-foot" ]
            ]
            [ HH.button
                [ HP.classes [ HH.ClassName "button is-success" ]
                , HE.onClick (HE.input_ Click_Settings_Save)
                , HP.disabled isDisabled_SaveButton 
                ]
                [ HH.text "Save changes" ]  
            , HH.button
                [ HP.classes [ HH.ClassName "button" ]
                , HE.onClick $ HE.input_ $ Click_Settings_Cancel isPendingChanges
                ]
                [ HH.text "Cancel" ]                      
            , HH.button
                [ HP.classes [ HH.ClassName "button is-warning" ] -- todo float all the way to the right
                , HE.onClick (HE.input_ Click_Settings_Reset)
                , HP.disabled isDisabled_ResetButton  
                ]
                [ HH.text "Reset to Defaults" ]  
            ]


    selectedColor :: Color
    selectedColor =
        state.settings.selectedColor


    players :: EditPlayers
    players =
        state.settings.players


    playerForActiveSetting :: EditPlayer
    playerForActiveSetting =
        getItemColored state.settings.selectedColor players


    editRec :: EditPlayerTypeRec
    editRec =
        x where (EditPlayer _ x) = playerForActiveSetting


    isDisabled_SaveButton :: Boolean
    isDisabled_SaveButton =
        not isPendingChanges


    isDisabled_ResetButton :: Boolean
    isDisabled_ResetButton =
        HLPR.isGameEnded state 
        ||  
        (toPlayers players == DFLT.defaultPlayers)


    isDisabled_PlayerType :: Boolean
    isDisabled_PlayerType =
        HLPR.isGameStarted state


    isSelected_ComputerDetails_SearchDepth :: Boolean
    isSelected_ComputerDetails_SearchDepth =
        not editRec.computer_isRandomPick   


    isSelected_PersonDetails_SearchDepth :: Boolean
    isSelected_PersonDetails_SearchDepth = 
        editRec.person_isAutoSuggest


    isSelected_Computer :: Boolean
    isSelected_Computer =
        editRec.playerType == EditComputer


    isSelected_Person :: Boolean
    isSelected_Person =
        editRec.playerType == EditPerson


    body_PersonDetails_section_classes :: String
    body_PersonDetails_section_classes =
        if editRec.person_isAutoSuggest then 
            "pl1 w5" <> CC.moveSquareBorder_Suggested 
        else 
            ""

    isPendingChanges :: Boolean
    isPendingChanges =
        (not $ HLPR.isGameEnded state)
        &&
        ( (HLPR.sequenceStateRecOn state).players /= toPlayers players )          