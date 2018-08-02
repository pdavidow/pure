module ConfirmModalHTML
    ( confirmModal_HTML )
    where
      
import Prelude
import Display (isActiveClass_Tag)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Data.Boolean (kind Boolean)
import Query (Query)


confirmModal_HTML :: Boolean -> String -> H.Action Query -> H.Action Query -> H.ComponentHTML Query -- https://functionalprogramming.slack.com/archives/C717K38CE/p1532459248000151
confirmModal_HTML isActive operationName okAction cancelAction =
-- confirmModal_HTML :: Boolean -> Boolean -> String -> H.Action Query -> H.Action Query -> H.ComponentHTML Query -- https://functionalprogramming.slack.com/archives/C717K38CE/p1532459248000151
-- confirmModal_HTML isActive isLoading operationName okAction cancelAction =
    HH.div
        [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass_Tag isActive)  ]
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
                    [ HH.text $ "Confirm: " <> operationName <> "?"]                        
                ]
            , HH.footer 
                [ HP.classes [ HH.ClassName "modal-card-foot" ]
                ]
                [ HH.button
                    [ HP.classes [ HH.ClassName $ "button is-success" ] --  "<> (isLoading_Tag isLoading)"  ] todo
                    , HE.onClick (HE.input_ okAction)
                    ]
                    [ HH.text "Ok" ]                        
                , HH.button
                    [ HP.classes [ HH.ClassName "button" ]
                    , HE.onClick (HE.input_ cancelAction)
                    ]
                    [ HH.text "Cancel" ]                          
                ]
            ]
        ]