module BoardComponent
    ( State
    , Effects
    , Status_StartRestart(..)
    , component
    )
    where

import Prelude

import BlackWhite (getItemBlack, getItemWhite, getItemColored, setItemColored)
import Board (Move, boardElems, movePosition)
import BoardSize (boardSize)
import ConfirmDialog (confirmDialog)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import DOM.HTML.Indexed.InputType as DOMT
import Data.List (List(Nil), elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Monoid (guard)
import Disk (Color(..), toggleColor)
import Display (Filled_NotStartedGame_DisplaySquare(..), Move_DisplaySquare(..), FilledSelf_DisplaySquare(..), FilledOpponent_DisplaySquare(..), Filled_EndedGame_DisplaySquare(..), Tagged_DisplaySquare(..), toDisplaySquare, toPosition, placedDisksStatus, status, potentialDiskClassesForColor, flipDiskClassesForColor, gameOver_Emphasis, placedDiskClassesForColor, unusedDiskClassesForColor, isActiveClass_Tag, nameForStartRestartButton)
import DisplayConstants as DC
import GameHistory (GameHistory, makeHistory, undoHistoryOnce)
import GameState (NextMoves, Tagged_GameState, board_FromTaggedGameState, nextMoves_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState, unusedDiskCounts_FromTaggedGameState, isStartGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Lib (setCssProp, haskellRange)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), Players, isPlayer_Computer, isPlayer_Person, isComputerVsComputer)
import Position (Position)
import Sequencer (moveSequence, advanceHistory, mbSuggestedMove, mbCurrentPlayer, unsafe_CurrentPlayer)
import SettingsDefaults as DFLT
import Type.Data.Boolean (kind Boolean)
import UnusedDiskCount (UnusedDiskCounts, maxDiskCount)
import Query (Query(..))

data Status_StartRestart 
    = NotStarted
    | Started 
    | AwaitingRestart


type State = 
    { players :: Players
    , gameHistory :: GameHistory
    , mb_Focused_MoveSquare :: Maybe Move_DisplaySquare
    , mb_MouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
    , mb_SuggestedMove :: Maybe Move
    , isShow_FlipCounts :: Boolean
    , isShow_ResetToDefaultsModal :: Boolean
    , isActive_SettingsModal :: Boolean
    , isImminentGameStart :: Boolean
    , isAwaitingConfirm_ResetSettingsToDefaults :: Boolean
    , status_StartRestart :: Status_StartRestart
    , activeSettingsColor :: Color
    }

type Effects eff = ( dom :: DOM, console :: CONSOLE, random :: RANDOM | eff )


derive instance eqStartRestart :: Eq Status_StartRestart   


component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
component =
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
 
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


    isUndoable :: GameHistory -> Boolean
    isUndoable history =
        isJust $ undoHistoryOnce history


    maxUnusedDiskCountProp :: forall r i. HP.IProp r i
    maxUnusedDiskCountProp =
        HP.prop (HH.PropName "--maxUnusedDiskCount") maxDiskCount


    gameStateOn :: GameHistory -> Tagged_GameState
    gameStateOn history = 
        NE.last history


    render :: State -> H.ComponentHTML Query
    render state =
        HH.div 
            ( guard isEvent_MouseUp_Anywhere
                [ HE.onMouseUp $ HE.input_ $ MouseUp_Anywhere 
                ] 
            )        
            [ HH.span
                [ HP.classes [ HH.ClassName "ml3 roboto" ]
                ]    
                [ HH.span
                    [ HP.classes [ HH.ClassName "b" ] -- todo "black bg_white hover_white hover_bg_black" effect doesn't work  
                    ]    
                    [ HH.text "OTHELLO" ] 
                , HH.button
                    [ HP.classes [ HH.ClassName "ml3 button is-small is-inverted is-outlined" ]
                    , HP.disabled $ (isGameStarted && isStartGameState gameState)
                    , HE.onMouseEnter $ HE.input_ $ MouseEnter_StartStopButton
                    , HE.onMouseLeave $ HE.input_ $ MouseLeave_StartStopButton                    
                    , HE.onClick $ HE.input_ Click_GameStartRestart
                    ] 
                    [ HH.text $ nameForStartRestartButton isGameStarted state.players]
                , HH.a
                    [ HP.classes [ HH.ClassName "ml3" ] -- button modal-button
                    --, HP.prop (HH.PropName "data-target") DC.modalSettingsId 
                    , HPA.hasPopup "true"
                    , HE.onClick $ HE.input_ Click_Open_Settings
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
            , HH.div
                [ HP.classes [ HH.ClassName "ml3" ]  
                ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "mt2 board-grid" ]
                    , setCssProp "--boardSize" $ show boardSize
                    ] 
                    ( map renderSquare squares )
                , HH.div            
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
                , HH.span
                    [ HP.classes [ HH.ClassName "mt2" ] -- todo unused controls-grid"
                    ] $
                    [ HH.button
                        [ HP.classes [ HH.ClassName "" ]
                        , HP.enabled $ isUndoable state.gameHistory
                        , HE.onClick $ HE.input_ Undo
                        ]
                        [ HH.text "Undo" ] 
                    , HH.button
                        [ HP.classes [ HH.ClassName "ml4" ]
                        , HE.onClick $ HE.input_ Click_FlipCounts
                        , HP.disabled $ not isGameStarted
                        ]
                        [ HH.text "Flip Counts" ]   
                    ]
                    <> 
                    guard (isComputerVsComputer state.players) [ HH.button
                        [ HP.classes [ HH.ClassName "ml4" ]
                        , HE.onClick $ HE.input_ Click_ComputerProceed
                        , HP.disabled $ not isGameStarted
                        ]
                        [ HH.text "Computer Proceed" ]  
                    ]  
                    <>
                    [ HH.span
                        [ HP.classes [ HH.ClassName $ "ml4 " <> gameOver_Emphasis gameState ] 
                        ]
                        [ HH.text $ placedDisksStatus isGameStarted gameState]     
                    ]                               
                , HH.div
                    [ HP.classes [ HH.ClassName "mt2 f3 lh-copy b" ]
                    ] 
                    [ HH.text $ status state.isImminentGameStart isGameStarted state.players gameState ]  
                ]   
            , HH.div -- todo refactor into separate module
                [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass_Tag state.isActive_SettingsModal)  ]
                --, HP.id_ DC.modalSettingsId 
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
            , confirmDialog isShow_RestartModal "New game" Click_Confirm_Restart Click_Cancel_Restart
            , confirmDialog isShow_ResetToDefaultsModal "Reset to Defaults" Click_Confirm_ResetToDefaults Click_Cancel_ResetToDefaults
            ]
        where 

        isGameStarted :: Boolean
        isGameStarted =
            state.status_StartRestart /= NotStarted


        isEvent_MouseUp_Anywhere :: Boolean
        isEvent_MouseUp_Anywhere =
            case mbCurrentPlayer state.players gameState of
                Just player -> isPlayer_Person player
                Nothing -> false


        isShow_RestartModal :: Boolean
        isShow_RestartModal =
            state.status_StartRestart == AwaitingRestart


        isShow_ResetToDefaultsModal :: Boolean
        isShow_ResetToDefaultsModal =
            state.isShow_ResetToDefaultsModal


        playerForActiveSetting :: Player
        playerForActiveSetting =
            getItemColored (state.activeSettingsColor) state.players


        unusedDiskCounts :: UnusedDiskCounts
        unusedDiskCounts =
            unusedDiskCounts_FromTaggedGameState gameState


        mbMoveColor :: Maybe Color
        mbMoveColor =
            mbNextMoveColor_FromTaggedGameState gameState


        gameState :: Tagged_GameState
        gameState = 
            gameStateOn state.gameHistory


        squares :: Array Tagged_DisplaySquare
        squares =
            (boardElems $ board_FromTaggedGameState gameState)
                # map (toDisplaySquare gameState isGameStarted)  


        moves :: NextMoves
        moves = 
            nextMoves_FromTaggedGameState gameState


        renderSquare :: Tagged_DisplaySquare -> H.ComponentHTML Query
        renderSquare taggedDisplaySquare =
            HH.figure
                squareProps
                [ HH.div
                    [ HP.classes [ HH.ClassName diskClasses ] ]
                    diskChildren
                ] 
            where

            squareProps =
                case taggedDisplaySquare of
                    Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
                        [ HP.classes 
                            [ HH.ClassName $ DC.basicGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent 
                        ]                    

                    Tagged_Filled_NotStartedGame_DisplaySquare _ -> 
                        [ HP.classes 
                            [ HH.ClassName $ DC.fillableGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]                     

                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.basicGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]
 
                    Tagged_Move_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.fillableGridItem <> squareProps__Move_DisplaySquare x ]                       
                        , HE.onMouseEnter $ HE.input_ $ MouseEnter_MoveSquare x
                        , HE.onMouseLeave $ HE.input_ $ MouseLeave_MoveSquare                      
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]
                        <> guard (isPlayer_Person $ unsafe_CurrentPlayer state.players gameState)
                        [ HE.onMouseDown $ HE.input_ $ MouseDown_MoveSquare x
                        , HE.onMouseUp $ HE.input_ $ MouseUp_MoveSquare x                                
                        ] 

                    Tagged_FilledSelf_DisplaySquare _ ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.fillableGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]                    
            
                    Tagged_FilledOpponent_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.fillableGridItem <> 
                                if isOutflankSquare_FocusedMoveSquare then 
                                    DC.outflankSquareColor_FocusedMoveSquare <> 
                                    DC.outflankSquareBorder_FocusedMoveSquare
                                else if isOutflankSquare_FocusedFilledOpponentSquare then 
                                    DC.outflankSquareColor_FocusedFilledOpponentSquare <> 
                                    DC.outflankSquareBorder_FocusedFilledOpponentSquare                                    
                                else 
                                    DC.defaultSquareColor <> 
                                    DC.squareBorder_Default
                            ]
                        , HE.onMouseEnter $ HE.input_ $ MouseEnter_FilledOpponentSquare x
                        , HE.onMouseLeave $ HE.input_ $ MouseLeave_FilledOpponentSquare                          
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ] 

                    Tagged_Empty_EndedGame_DisplaySquare _ ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.basicGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec) ->
                        let
                            squareColor = 
                                case rec.mbIsWinningColor of
                                    Nothing -> DC.tieEndedSquareColor 
                                    
                                    Just boolean ->
                                        case boolean of
                                            true  -> DC.winEndedSquareColor 
                                            false -> DC.defaultSquareColor
                        in
                            [ HP.classes 
                                [ HH.ClassName $ DC.fillableGridItem <> 
                                    squareColor <> 
                                    DC.squareBorder_Default
                                ]
                            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                            ] 


            squareProps__Move_DisplaySquare :: Move_DisplaySquare -> String
            squareProps__Move_DisplaySquare moveSquare =
                backgroundColorProps <> borderProps
                where
                    backgroundColorProps =
                        if isMove_FocusedMoveSquare moveSquare then  
                            DC.moveSquareColor_FocusedMoveSquare 
                        else if isMove_FocusedFilledOpponentSquare moveSquare then  
                            DC.moveSquareColor_FocusedFilledOpponentSquare
                        else
                            DC.moveSquareColor                    

                    borderProps = 
                        if isSuggestedMoveSquare moveSquare then
                            DC.moveSquareBorder_Suggested
                        else 
                            DC.moveSquareBorder_NonSuggested


            isSuggestedMoveSquare :: Move_DisplaySquare -> Boolean
            isSuggestedMoveSquare (Move_DisplaySquare rec) =
                case state.mb_SuggestedMove of
                    Nothing -> false
                    Just move -> move == rec.move


            isMove_FocusedMoveSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedMoveSquare moveSquare =
                Just moveSquare == state.mb_Focused_MoveSquare 


            isMove_FocusedFilledOpponentSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedFilledOpponentSquare (Move_DisplaySquare rec) =
                elem (movePosition rec.move) state.moves_FocusedFilledOpponentSquare


            isOutflankSquare_FocusedMoveSquare :: Boolean
            isOutflankSquare_FocusedMoveSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare


            isOutflankSquare_MouseDownMoveSquare :: Boolean
            isOutflankSquare_MouseDownMoveSquare =
                isOutflankSquare_FocusedMoveSquare && isJust state.mb_MouseDown_MoveSquare


            isOutflankSquare_FocusedFilledOpponentSquare :: Boolean
            isOutflankSquare_FocusedFilledOpponentSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare


            diskClasses :: String
            diskClasses = 
                case taggedDisplaySquare of 
                    Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
                        ""                 

                    Tagged_Filled_NotStartedGame_DisplaySquare (Filled_NotStartedGame_DisplaySquare rec)  -> 
                        if state.isImminentGameStart then
                            placedDiskClassesForColor rec.color
                        else
                            ""

                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        ""

                    Tagged_Move_DisplaySquare x ->           
                        if isMove_FocusedMoveSquare x && isJust state.mb_MouseDown_MoveSquare then
                            potentialDiskClassesForColor $ unsafePartial fromJust $ mbMoveColor
                        else
                            ""                            

                    Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)  -> 
                        placedDiskClassesForColor rec.color

                    Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)  -> 
                        if isOutflankSquare_MouseDownMoveSquare then
                            flipDiskClassesForColor $ toggleColor rec.color
                        else
                            placedDiskClassesForColor $ rec.color 

                    Tagged_Empty_EndedGame_DisplaySquare _ ->       
                        ""                            
                        
                    Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec) -> 
                        placedDiskClassesForColor rec.color                    


            diskChildren :: Array (H.ComponentHTML Query)
            diskChildren =
                let
                    mbFlipCount = 
                        if state.isShow_FlipCounts then
                            case taggedDisplaySquare of 
                                Tagged_Empty_NotStartedGame_DisplaySquare _                                -> Nothing
                                Tagged_Filled_NotStartedGame_DisplaySquare _                               -> Nothing
                                Tagged_Empty_NonMove_DisplaySquare _                                       -> Nothing
                                Tagged_Move_DisplaySquare _                                                -> Nothing                                    
                                Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)             -> Just rec.flipCount
                                Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)     -> Just rec.flipCount
                                Tagged_Empty_EndedGame_DisplaySquare _                                     -> Nothing                                                         
                                Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec) -> Just rec.flipCount
                        else
                            Nothing
                in
                    case mbFlipCount of
                        Nothing ->
                            []
                        Just flipCount ->
                            [ HH.div
                                [ HP.classes [ HH.ClassName DC.flipCountText ]
                                ]
                                [ HH.text $ show flipCount  ]      
                            ]


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

            