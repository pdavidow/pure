module GameComponent
    ( Effects
    , component
    )
    where

import Prelude

import BlackWhite (getItemBlack, getItemWhite, setItemColored)
import BoardHTML (board_HTML)
import ConfirmModalHTML (confirmModal_HTML)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.Classy.Event (preventDefault)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Disk (Color(..))
import Display (Move_DisplaySquare(..), FilledOpponent_DisplaySquare(..), placedDisksStatus, status, gameOver_Emphasis, unusedDiskClassesForColor)
import GameHistory (makeHistory, undoHistoryOnce)
import GameState (unusedDiskCounts_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Lib (haskellRange)
import NavbarHTML (navbar_HTML)
import Player (Player(..), isPlayer_Person, isComputerVsComputer)
import Query (Query(..))
import Sequencer (moveSequence, advanceHistory, mbSuggestedMove, mbCurrentPlayer)
import SettingsDefaults as DFLT
import SettingsModalHTML (settingsModal_HTML)
import State (State)
import StatusStartRestart (Status_StartRestart(..))
import Type.Data.Boolean (kind Boolean)
import UnusedDiskCount (UnusedDiskCounts, maxDiskCount)
import ViewLib (setCssProp)


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
 
    -- todo move to State module
    initialState :: State 
    initialState = 
        { players: players
        , gameHistory: gameHistory
        , mb_Focused_MoveSquare: Nothing
        , mb_MouseDown_MoveSquare: Nothing
        , outflanks_FocusedMoveSquare: Nil
        , moves_FocusedFilledOpponentSquare: Nil        
        , outflanks_FocusedFilledOpponentSquare: Nil
        , mb_SuggestedMove: mb_SuggestedMove
        , isShow_FlipCounts: false
        , isShow_ResetToDefaultsModal: false
        , isActive_SettingsModal: false
        , isImminentGameStart: false
        , isAwaitingConfirm_ResetSettingsToDefaults: false
        , status_StartRestart: NotStarted
        , activeSettingsColor: Black           
        }
        where 
            players = DFLT.defaultPlayers  
            gameHistory = makeHistory 
            mb_SuggestedMove = mbSuggestedMove players gameHistory


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
                , HH.div   -- todo break out into UnusedDisk_HTML module                 
                -- [ HH.div   -- todo break out into UnusedDisk_HTML module          
                    [ HP.classes [ HH.ClassName "unusedDiskGrids-grid" ]
                    , setCssProp "--maxUnusedDiskCount" $ show maxDiskCount 
                    ]  
                    [ HH.div            
                        [ HP.classes [ HH.ClassName "unusedDisk-grid" ]
                        ]
                        ( map (const $ renderUnusedDisk Black) $ haskellRange 1 $ getItemBlack unusedDiskCounts) -- todo use repeat ?
                    , HH.div            
                        [ HP.classes [ HH.ClassName "unusedDisk-grid" ] 
                        ]                
                        ( map (const $ renderUnusedDisk White) $ haskellRange 1 $ getItemWhite unusedDiskCounts) -- todo use repeat ? 
                    ] 
                , HH.span -- todo break out into Dashboard_HTML module
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
                    <> 
                    guard (isComputerVsComputer state.players) [ HH.button
                        [ HP.classes [ HH.ClassName "ml4" ]
                        , HE.onClick $ HE.input_ Click_ComputerProceed
                        , HP.disabled $ not $ HLPR.isGameStarted state
                        ]
                        [ HH.text "Computer Proceed" ]  
                    ]  
                    <>
                    [ HH.span
                        [ HP.classes [ HH.ClassName $ "ml4 " <> (gameOver_Emphasis $ HLPR.gameState state) ] 
                        ]
                        [ HH.text $ placedDisksStatus (HLPR.isGameStarted state) $ HLPR.gameState state]     
                    ]                               
                , HH.div -- todo break out into DashboardFooter_HTML module
                    [ HP.classes [ HH.ClassName "mt2 f3 lh-copy b" ]
                    ] 
                    [ HH.text $ status state.isImminentGameStart (HLPR.isGameStarted state) state.players $ HLPR.gameState state ]  
                ]   
            , settingsModal_HTML state
            , confirmModal_HTML isShow_RestartModal "New game" Click_Confirm_Restart Click_Cancel_Restart
            , confirmModal_HTML isShow_ResetToDefaultsModal "Reset to Defaults" Click_Confirm_ResetToDefaults Click_Cancel_ResetToDefaults
            ]
        where 

        isEvent_MouseUp_Anywhere :: Boolean
        isEvent_MouseUp_Anywhere =
            case mbCurrentPlayer state.players $ HLPR.gameState state of
                Just player -> isPlayer_Person player
                Nothing -> false


        isShow_RestartModal :: Boolean
        isShow_RestartModal =
            state.status_StartRestart == AwaitingRestart


        isShow_ResetToDefaultsModal :: Boolean
        isShow_ResetToDefaultsModal =
            state.isShow_ResetToDefaultsModal


        unusedDiskCounts :: UnusedDiskCounts
        unusedDiskCounts =
            unusedDiskCounts_FromTaggedGameState $ HLPR.gameState state


        renderUnusedDisk :: Color -> H.ComponentHTML Query
        renderUnusedDisk color =
            HH.figure
                [ HP.classes [ HH.ClassName $ unusedDiskClassesForColor color] ] 
                []


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

        Click_FlipCounts next -> do
            isShow_FlipCounts <- H.gets _.isShow_FlipCounts  
            H.modify (_ 
                { isShow_FlipCounts = not isShow_FlipCounts
                }
            )
            pure next

        Click_Open_Settings next -> do
            H.modify (_ 
                { isActive_SettingsModal = true
                }
            )        
            pure next

        Click_Close_Settings next -> do
            H.modify (_ 
                { isActive_SettingsModal = false
                }
            )        
            pure next

        Click_Settings color next -> do
            H.modify (_ 
                { activeSettingsColor = color
                }
            )        
            pure next

        Click_Settings_Computer color next -> do      
            players <- H.gets _.players 
            let player' = Player color DFLT.defaultPlayerType_Computer  
            let players' = setItemColored color players player'

            H.modify (_ 
                { players = players' 
                }
            )  
            pure next

        Click_Settings_Person color next -> do     
            players <- H.gets _.players 
            let player' = Player color DFLT.defaultPlayerType_Person            
            let players' = setItemColored color players player'

            H.modify (_ 
                { players = players'
                }
            )  
            pure next

        Click_ComputerProceed next -> do
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

        Click_Cancel_Restart next -> do
            H.modify (_ 
                { status_StartRestart = Started
                }
            )         
            pure next

        Click_Confirm_Restart next -> do 
            -- Keep player settings

            players <- H.gets _.players 
            activeSettingsColor <- H.gets _.activeSettingsColor 

            H.put initialState   
            H.modify (_ 
                { players = players
                , activeSettingsColor = activeSettingsColor
                }
            )                    
            pure next

        Click_ResetSettingsToDefaults next -> do
            H.modify (_ 
                { isShow_ResetToDefaultsModal = true 
                }
            )                
            pure next

        Click_Confirm_ResetToDefaults next -> do
            H.modify (_ 
                { players = DFLT.defaultPlayers 
                , isShow_ResetToDefaultsModal = false
                }
            )                
            pure next

        Click_Cancel_ResetToDefaults next -> do
            H.modify (_ 
                { isShow_ResetToDefaultsModal = false
                }
            )                
            pure next            

            