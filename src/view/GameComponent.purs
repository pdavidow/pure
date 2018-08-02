module GameComponent
    ( Effects
    , component
    )
    where

import Prelude

import BlackWhite (getItemColored, setItemColored)
import BoardHTML (board_HTML)
import ConfirmModalHTML (confirmModal_HTML)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.Classy.Event (preventDefault)
import DashboardFooterHTML (dashboardFooter_HTML)
import DashboardHTML (dashboard_HTML)
import Data.List (List(Nil))
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Display (Move_DisplaySquare(..), FilledOpponent_DisplaySquare(..))
import GameState (Tagged_GameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import History (swapLast, undoHistoryOnce)
import NavbarHTML (navbar_HTML)
import Player (isPlayer_Person)
import Query (Query(..))
import SequenceState (SequenceStateRec, SequenceState(..), seqRec) 
import Sequencer (moveSequence, advanceHistoryFromPersonMove, mbCurrentPlayer)
import Settings (EditPlayer(..), defaultSettingsRec, settingsRecOn, toPlayers) 
import SettingsModalHTML (settingsModal_HTML)
import State (State, initialState)
import StatusStartRestart (Status_StartRestart(..))
import Type.Data.Boolean (kind Boolean)
import UnusedDiskHTML (unusedDisk_HTML)


type Effects eff = ( dom :: DOM, console :: CONSOLE, random :: RANDOM | eff )   


component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
component =
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
 
    render :: State -> H.ComponentHTML Query
    render state =
        HH.div 
            ( guard isEvent_MouseUp_Anywhere
                [ HE.onMouseUp $ HE.input_ $ MouseUp_Anywhere ]           
            ) $   
            [ navbar_HTML state
            , HH.div
                [ HP.classes [ HH.ClassName "ml3" ]  
                ]
                [ board_HTML state
                , unusedDisk_HTML state
                , dashboard_HTML state   
                , dashboardFooter_HTML state                          
                ]   
            ] <> modals 
        where 

        modals :: Array (H.ComponentHTML Query)
        modals = 
            [ settingsModal_HTML state -- parent such as this, must come first in order to be overlaid by child modal
            , confirmModal_HTML isShowModal_Restart "New game" Click_Confirm_Restart Click_Cancel_Restart 
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Save "Save Changes" Click_Settings_Save_Confirm Click_Settings_Save_Cancel
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Cancel "Cancel Changes" Click_Settings_Cancel_Confirm Click_Settings_Cancel_Cancel
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Reset "Reset to Defaults" Click_Settings_Reset_Confirm Click_Settings_Reset_Cancel
            ]


        isEvent_MouseUp_Anywhere :: Boolean
        isEvent_MouseUp_Anywhere = 
            maybe false isPlayer_Person $ mbCurrentPlayer srec.players gameState 


        isShowModal_Restart :: Boolean
        isShowModal_Restart =
            state.status_StartRestart == AwaitingRestart


        gameState :: Tagged_GameState
        gameState = srec.game        


        srec :: SequenceStateRec 
        srec = HLPR.sequenceStateRecOn state     


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = 
        case _ of    
            MouseEnter_StartStopButton next -> do
                H.modify (_ 
                    { isImminentGameStart = true
                    }
                )
                pure next

            MouseLeave_StartStopButton next -> do
                H.modify (_ 
                    { isImminentGameStart = false
                    }
                )
                pure next            

            MouseEnter_MoveSquare x@(Move_DisplaySquare rec) next -> do
                H.modify (_ 
                    { outflanks_FocusedMoveSquare = rec.outflanks
                    , mb_Focused_MoveSquare = Just x 
                    }
                )
                pure next

            MouseLeave_MoveSquare next -> do
                H.modify (_ 
                    { outflanks_FocusedMoveSquare = Nil
                    , mb_Focused_MoveSquare = Nothing
                    }
                )
                pure next

            MouseDown_MoveSquare x next -> do
                H.modify (_ 
                    { mb_MouseDown_MoveSquare = Just x
                    }
                )
                pure next

            MouseUp_MoveSquare x@(Move_DisplaySquare rec) next -> do
                -- Events bubble up from inner elements to outer elements, so inner event handlers will be run first

                mouseDown_MoveSquare <- H.gets _.mb_MouseDown_MoveSquare

                when (mouseDown_MoveSquare == Just x) do    
                    history <- H.gets _.history   
                    history' <- liftEff $ advanceHistoryFromPersonMove history rec.move    
                    H.modify (_  { history = history' } )

                pure next       

            MouseUp_Anywhere next -> do
                H.modify (_ 
                    { mb_Focused_MoveSquare = Nothing
                    , mb_MouseDown_MoveSquare = Nothing                
                    , outflanks_FocusedMoveSquare = Nil
                    }
                )    
                pure next                    

            MouseEnter_FilledOpponentSquare (FilledOpponent_DisplaySquare rec) next -> do
                H.modify (_ 
                    { outflanks_FocusedFilledOpponentSquare = rec.outflanks
                    , moves_FocusedFilledOpponentSquare = rec.moves
                    }
                )            
                pure next

            MouseLeave_FilledOpponentSquare next -> do
                H.modify (_ 
                    { outflanks_FocusedFilledOpponentSquare = Nil
                    , moves_FocusedFilledOpponentSquare = Nil
                    }
                )
                pure next

            Click_GameStartRestart next -> do    
                status_StartRestart <-  H.gets _.status_StartRestart 
                if status_StartRestart == NotStarted
                    then do 
                        H.modify (_ { status_StartRestart = Started })                
                    else if status_StartRestart == Started   
                        then do
                            H.modify (_ { status_StartRestart = AwaitingRestart })
                        else do
                            pure unit

                status_StartRestart' <-  H.gets _.status_StartRestart
                if status_StartRestart' == Started
                    then do   
                        history <- H.gets _.history         
                        history' <- liftEff $ moveSequence history

                        H.modify (_ 
                            { isImminentGameStart = false
                            , history = history'
                            }
                        )  
                    else do
                        pure unit                               

                pure next

            Click_Cancel_Restart next -> do
                H.modify (_ 
                    { status_StartRestart = Started
                    }
                )         
                pure next

            Click_Confirm_Restart next -> do 
                history <- H.gets _.history
                let sqState = NE.last history  
                let keepPlayers = (seqRec sqState).players             

                H.put initialState   
                history' <- H.gets _.history
                let sqState' = NE.last history' 
                let sqState'' = SequenceState (seqRec sqState') {players = keepPlayers}
                H.modify (_ 
                    { history = swapLast history' sqState''
                    }
                )                    
                pure next

            Click_Settings_Open next -> do
                history <- H.gets _.history
                let sqState = NE.last history 

                H.modify (_ 
                    { settings = settingsRecOn (seqRec sqState).players
                    , isShowModal_Settings = true
                    }
                )        
                pure next

            Click_Settings_Save next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Save = true 
                    }
                )        
                pure next            

            Click_Settings_Save_Confirm next -> do
                settings <- H.gets _.settings
                let editPlayers = settings.players 
                history <- H.gets _.history
                let sqState = NE.last history                 
                let sqState' = SequenceState (seqRec sqState) {players = toPlayers editPlayers}

                H.modify (_ 
                    { history = swapLast history sqState'
                    , isShowModal_Confirm_Settings_Save = false
                    , isShowModal_Settings = false
                    }
                )    

                sqState'' <- H.get 
                when (HLPR.isGameStarted sqState'') do    
                    history' <- H.gets _.history  -- retreive to play it safe         
                    history'' <- liftEff $ moveSequence history'

                    H.modify (_ 
                        { history = history''
                        }
                    )              

                pure next

            Click_Settings_Save_Cancel next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Save = false
                    }
                )                
                pure next 

            Click_Settings_Cancel isPendingChanges next -> do
                if isPendingChanges
                    then do
                        H.modify (_ 
                            { isShowModal_Confirm_Settings_Cancel = true
                            }
                        )
                    else do
                        H.modify (_ 
                            { isShowModal_Settings = false
                            }   
                        )       
                pure next

            Click_Settings_Cancel_Confirm next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Cancel = false
                    , isShowModal_Settings = false
                    }
                )                
                pure next

            Click_Settings_Cancel_Cancel next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Cancel = false
                    }
                )                
                pure next 

            Click_Settings_Reset next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Reset = true 
                    }
                )                
                pure next

            Click_Settings_Reset_Confirm next -> do
                H.modify (_ 
                    { settings = defaultSettingsRec
                    , isShowModal_Confirm_Settings_Reset = false              
                    }
                )                
                pure next

            Click_Settings_Reset_Cancel next -> do
                H.modify (_ 
                    { isShowModal_Confirm_Settings_Reset = false
                    }
                )                
                pure next 

            Click_Settings_selectedColor color next -> do 
                settings <- H.gets _.settings

                H.modify (_ 
                    { settings = settings {selectedColor = color}
                    }
                )        
                pure next

            ModifySettings color f next -> do
                settings <- H.gets _.settings
                let editPlayers = settings.players 
                let (EditPlayer _ rec) = getItemColored color editPlayers
                let editPlayer = EditPlayer color $ f rec
                let editPlayers' = setItemColored color editPlayers editPlayer

                H.modify (_ 
                    { settings = settings {players = editPlayers'}
                    }
                ) 
                pure next          

            Undo next -> do
                history <- H.gets _.history
                let mbHistory' = undoHistoryOnce history

                case mbHistory' of
                    Nothing -> do
                        pure next

                    Just history' -> do                    
                        H.modify (_ 
                            { history = history'
                            }
                        )
                        pure next    

            Click_FlipCounts next -> do
                isShow_FlipCounts <- H.gets _.isShow_FlipCounts  
                H.modify (_ 
                    { isShow_FlipCounts = not isShow_FlipCounts
                    }
                )
                pure next

            Click_ComputerStep next -> do
                history <- H.gets _.history              
                history' <- liftEff $ moveSequence history

                H.modify (_ 
                    { history = history'
                    }
                )         
                pure next

            PreventDefault event next -> do
                H.liftEff $ preventDefault event
                pure next