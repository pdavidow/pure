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
import Data.Maybe (Maybe(..), maybe) 
import Data.Monoid (guard)
import Display (Move_DisplaySquare(..), FilledOpponent_DisplaySquare(..))
import GameHistory (undoHistoryOnce)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import NavbarHTML (navbar_HTML)
import Player (isPlayer_Person)
import Query (Query(..))
import Sequencer (moveSequence, advanceHistory, mbSuggestedMove, mbCurrentPlayer)
import SettingsDefaults as DFLT
import SettingsModalHTML (settingsModal_HTML)
import State (State, initialState)
import StatusStartRestart (Status_StartRestart(..))
import Type.Data.Boolean (kind Boolean)
import UnusedDiskHTML (unusedDisk_HTML)
import Settings (EditPlayer(..), toEditPlayers, toPlayers)

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
            )        
            [ navbar_HTML state
            , HH.div
                [ HP.classes [ HH.ClassName "ml3" ]  
                ]
                [ board_HTML state
                , unusedDisk_HTML state
                , dashboard_HTML state   
                , dashboardFooter_HTML state                          
                ]   

            , settingsModal_HTML state            
            , confirmModal_HTML isShowModal_Restart "New game" Click_Confirm_Restart Click_Cancel_Restart
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Save "Save Changes" Click_Settings_Save_Confirm Click_Settings_Save_Cancel
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Cancel "Cancel Changes" Click_Settings_Cancel_Confirm Click_Settings_Cancel_Cancel
            , confirmModal_HTML state.isShowModal_Confirm_Settings_Reset "Reset to Defaults" Click_Settings_Reset_Confirm Click_Settings_Reset_Cancel
            ]
        where 

        isEvent_MouseUp_Anywhere :: Boolean
        isEvent_MouseUp_Anywhere = 
            maybe false isPlayer_Person $ mbCurrentPlayer state.players $ HLPR.gameState state 


        isShowModal_Restart :: Boolean
        isShowModal_Restart =
            state.status_StartRestart == AwaitingRestart


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of    
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
            -- https://functionalprogramming.slack.com/archives/C717K38CE/p1531977189000117

            -- Events bubble up from inner elements to outer elements, so inner event handlers will be run first
            mouseDown_MoveSquare <- H.gets _.mb_MouseDown_MoveSquare

            if mouseDown_MoveSquare == Just x  
                then do
                    gameHistory <- H.gets _.gameHistory             
                    players <- H.gets _.players   
                    gameHistory' <- liftEff $ advanceHistory players gameHistory rec.move   

                    H.modify (_ 
                            { gameHistory = gameHistory'
                            , mb_SuggestedMove = mbSuggestedMove players gameHistory'
                        }
                    )
                else 
                    pure unit
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
                    gameHistory <- H.gets _.gameHistory             
                    players <- H.gets _.players   
                    gameHistory' <- liftEff $ moveSequence players gameHistory

                    H.modify (_ 
                        { isImminentGameStart = false
                        , gameHistory = gameHistory'
                        , mb_SuggestedMove = mbSuggestedMove players gameHistory'
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
            -- Keep settings

            players <- H.gets _.players 
            settings <- H.gets _.settings

            H.put initialState   
            H.modify (_ 
                { players = players
                , settings = settings
                }
            )                    
            pure next


        Click_Settings_Open next -> do
            players <- H.gets _.players  
            let editPlayers = toEditPlayers players

            H.modify (_ 
                { settings {players = editPlayers}
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
            editPlayers <- H.gets _.settings.players 
            let players = toPlayers editPlayers

            H.modify (_ 
                { players = players
                , isShowModal_Confirm_Settings_Save = false
                , isShowModal_Settings = false
                }
            )    

            state <- H.get 
            when (HLPR.isGameStarted state) do
                gameHistory <- H.gets _.gameHistory             
                samePlayers <- H.gets _.players -- retreive to play it safe  
                gameHistory' <- liftEff $ moveSequence samePlayers gameHistory

                H.modify (_ 
                    { gameHistory = gameHistory'
                    , mb_SuggestedMove = mbSuggestedMove samePlayers gameHistory'
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
            let editPlayers = toEditPlayers DFLT.defaultPlayers

            H.modify (_ 
                { settings {players = editPlayers}
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
            H.modify (_ 
                { settings {selectedColor = color}
                }
            )        
            pure next

        ModifySettings color f next -> do
            editPlayers <- H.gets _.settings.players  
            let (EditPlayer _ rec) = getItemColored color editPlayers
            let editPlayer = EditPlayer color $ f rec
            let editPlayers' = setItemColored color editPlayers editPlayer
            H.modify (_ { settings {players = editPlayers'} })  
            pure next          

        Undo next -> do
            gameHistory <- H.gets _.gameHistory
            let mbGameHistory' = undoHistoryOnce gameHistory

            case mbGameHistory' of
                Nothing -> do
                    pure next

                Just gameHistory' -> do                    
                    players <- H.gets _.players
                    H.modify (_ 
                        { gameHistory = gameHistory'
                        , mb_SuggestedMove = mbSuggestedMove players gameHistory'
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
            gameHistory <- H.gets _.gameHistory             
            players <- H.gets _.players   
            gameHistory' <- liftEff $ moveSequence players gameHistory

            H.modify (_ 
                { gameHistory = gameHistory'
                }
            )         
            pure next

        PreventDefault event next -> do
            H.liftEff $ preventDefault event
            pure next