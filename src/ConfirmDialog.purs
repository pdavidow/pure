module ConfirmDialog
    (confirmDialog)
    where
      
import Prelude
import Display (isActiveClass_Tag)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Data.Boolean (kind Boolean)
import Query (Query)

-- todo
--confirmDialog :: forall t a. Boolean -> String -> Query a -> Query a -> H.ComponentHTML Query
confirmDialog isActive operationName okAction cancelAction =
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
                    [ HP.classes [ HH.ClassName "button is-success" ]
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