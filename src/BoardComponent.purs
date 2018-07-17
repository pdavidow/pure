module BoardComponent
    ( Query
    , State
    , component
    )
    where

import Prelude

import BlackWhite (makeBlackWhite, getItemBlack, getItemWhite, getItemColored, setItemColored)
import Board (Move, boardElems, movePosition)
import BoardSize (boardSize)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import DOM.HTML.Indexed.InputType as DOMT
import Data.Either (fromRight)
import Data.List (List(Nil), elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, isJust)
import Defaults as DFLT
import Disk (Color(..), toggleColor)
import Display (Empty_NotStartedGame_DisplaySquare(..), Filled_NotStartedGame_DisplaySquare(..), Move_DisplaySquare(..), FilledSelf_DisplaySquare(..), FilledOpponent_DisplaySquare(..), Filled_EndedGame_DisplaySquare(..), Tagged_DisplaySquare(..), toDisplaySquare, toPosition, placedDisksStatus, status, potentialDiskClassesForColor, flipDiskClassesForColor, gameOver_Emphasis, placedDiskClassesForColor, unusedDiskClassesForColor, isActiveClass)
import DisplayConstants as DC
import GameHistory (GameHistory, applyMoveOnHistory, makeHistory, undoHistoryOnce)
import GameState (NextMoves, Tagged_GameState, board_FromTaggedGameState, nextMoves_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState, unusedDiskCounts_FromTaggedGameState, isStartGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Lib (setCssProp, haskellRange)
import Partial.Unsafe (unsafePartial)
import Player (Player(..), Players, mbSuggestedMove, isPlayer_Computer, isPlayer_Person, isComputerVsComputer)
import Position (Position)
import Type.Data.Boolean (kind Boolean)
import UnusedDiskCount (UnusedDiskCounts, maxDiskCount)

data Query a
  = MouseEnter_StartStopButton a
  | MouseLeave_StartStopButton a
  | MouseEnter_MoveSquare Move_DisplaySquare a
  | MouseLeave_MoveSquare a
  | MouseDown_MoveSquare Move_DisplaySquare a
  | MouseUp_Anywhere a
  | MouseUp_MoveSquare Move_DisplaySquare a  
  | MouseEnter_FilledOpponentSquare FilledOpponent_DisplaySquare a
  | MouseLeave_FilledOpponentSquare a  
  | Click_FlipCounts a
  | Click_Open_Settings a
  | Click_Close_Settings a  
  | Click_Settings Color a
  | Click_Settings_Computer Color a
  | Click_Settings_Person Color a  
  | Click_GameStartStop a
  | Click_NewGame a
  | Click_Close_NewGameConfirm a 
  | PreventDefault Event a
  | Undo a
  | NullOp a

type State = 
    { players :: Players
    , gameHistory :: GameHistory
    , mb_Focused_MoveSquare :: Maybe Move_DisplaySquare
    , mb_MouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
    , mb_SuggestedMove :: Maybe Move
    , isShowFlipCounts :: Boolean
    , isActive_SettingsModal :: Boolean
    , isImminentGameStart :: Boolean
    , isGameStarted :: Boolean
    , isAwaitingNewGameConfirm :: Boolean
    , activeSettingsColor :: Color
    }

type TempSettingsState =
    { players :: Players
    }

type Effects eff = ( dom :: DOM | eff )


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
        , isShowFlipCounts: false
        , isActive_SettingsModal: false
        , isImminentGameStart: false
        , isGameStarted: false
        , isAwaitingNewGameConfirm: false
        , activeSettingsColor: Black           
        }
        where
        players = DFLT.defaultPlayers  
        gameHistory = makeHistory 
        mb_SuggestedMove = mbSuggestedMove players $ gameStateOn gameHistory


    isUndoable :: GameHistory -> Boolean
    isUndoable history =
        isJust $ undoHistoryOnce history


    maxUnusedDiskCountProp :: forall r i. HP.IProp r i
    maxUnusedDiskCountProp =
        HP.prop (HH.PropName "--maxUnusedDiskCount") maxDiskCount


    gameStateOn :: GameHistory -> Tagged_GameState
    gameStateOn history = 
        NE.last history


-- todo -- also new game, quit  
    render :: State -> H.ComponentHTML Query
    render state =
        HH.div
            [ HE.onMouseUp $ HE.input_ $ MouseUp_Anywhere 
            ]           
            [ HH.span
                [ HP.classes [ HH.ClassName "ml3 roboto" ]
                ]    
                [ HH.span
                    [ HP.classes [ HH.ClassName "b" ] -- todo "black bg_white hover_white hover_bg_black" effect doesn't work  
                    ]    
                    [ HH.text "OTHELLO" ] 
                , HH.button
                    [ HP.classes [ HH.ClassName "ml3 button is-small" ]
                    , HP.disabled $ (state.isGameStarted && isStartGameState gameState) || isComputerVsComputer state.players
                    , HE.onMouseEnter $ HE.input_ $ MouseEnter_StartStopButton
                    , HE.onMouseLeave $ HE.input_ $ MouseLeave_StartStopButton                    
                    , HE.onClick $ HE.input_ Click_GameStartStop
                    ] 
                    [ HH.text nameForStartStopButton ]
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
                , HH.a
                    [ HP.classes [ HH.ClassName "ml3" ] -- button modal-button
                    --, HP.prop (HH.PropName "data-target") DC.modalSettingsId 
                    , HPA.hasPopup "true"
                    , HE.onClick $ HE.input_ Click_Open_Settings
                    ]
                    [ HH.text "Settings" ]  
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
                    ] 
                    [ HH.button
                        [ HP.classes [ HH.ClassName "" ]
                        , HP.enabled $ isUndoable state.gameHistory
                        , HE.onClick $ HE.input_ Undo
                        ]
                        [ HH.text "Undo" ] 
                    , HH.button
                        [ HP.classes [ HH.ClassName "ml4" ]
                        , HE.onClick $ HE.input_ Click_FlipCounts
                        , HP.disabled $ not state.isGameStarted
                        ]
                        [ HH.text "Flip Counts" ]                               
                    , HH.span
                        [ HP.classes [ HH.ClassName $ "ml4 " <> gameOver_Emphasis gameState ] 
                        ]
                        [ HH.text $ placedDisksStatus state.isGameStarted gameState]     
                    ]                                    
                , HH.div
                    [ HP.classes [ HH.ClassName "mt2 f3 lh-copy b" ]
                    ]
                    [ HH.text $ status state.isGameStarted gameState ]     
                ]               
            , HH.div
                [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass state.isActive_SettingsModal)  ]
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
                                    [ HP.classes [ HH.ClassName $ isActiveClass $ state.activeSettingsColor == Black ]                            
                                    ]
                                    [ HH.a
                                        [ HE.onClick $ HE.input_ $ Click_Settings Black ]
                                        [ HH.text "Black" ]
                                    ]
                                , HH.li
                                    [ HP.classes [ HH.ClassName $ isActiveClass $ state.activeSettingsColor == White ]                            
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
                    ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName $ "modal" <> (isActiveClass state.isAwaitingNewGameConfirm)  ]
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
                            [ HH.text "Confirm: New game?"]                        
                        ]
                    , HH.footer 
                        [ HP.classes [ HH.ClassName "modal-card-foot" ]
                        ]
                        [ HH.button
                            [ HP.classes [ HH.ClassName "button is-success" ]
                            , HE.onClick (HE.input_ Click_NewGame)
                            ]
                            [ HH.text "Ok" ]                        
                        , HH.button
                            [ HP.classes [ HH.ClassName "button" ]
                            , HE.onClick (HE.input_ Click_Close_NewGameConfirm)
                            ]
                            [ HH.text "Cancel" ]                          
                        ]
                    ]
                ]
            ]
        where 

        nameForStartStopButton :: String
        nameForStartStopButton =
            if state.isGameStarted then
                if isComputerVsComputer state.players then
                    "----" -- blocking for now, web-worker to the rescue...
                else
                    "New Game"
            else
                "Start" 


        proceedOnGameStart :: forall a. (a -> Maybe (Query Unit)) -> a -> Maybe (Query Unit)
        proceedOnGameStart x =
            if state.isGameStarted then
                x
            else
                HE.input_ NullOp


        playerForActiveSetting :: Player
        playerForActiveSetting =
            getItemColored (state.activeSettingsColor) state.players


        unusedDiskCounts :: UnusedDiskCounts
        unusedDiskCounts =
            if state.isImminentGameStart || state.isGameStarted then
                unusedDiskCounts_FromTaggedGameState gameState
            else
                makeBlackWhite 0 0


        mbMoveColor :: Maybe Color
        mbMoveColor =
            mbNextMoveColor_FromTaggedGameState gameState


        gameState :: Tagged_GameState
        gameState = 
            gameStateOn state.gameHistory


        squares :: Array Tagged_DisplaySquare
        squares =
            (boardElems $ board_FromTaggedGameState gameState)
                # map (toDisplaySquare gameState state.isGameStarted)  


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
                        , HE.onMouseDown $ HE.input_ $ MouseDown_MoveSquare x
                        , HE.onMouseUp $ HE.input_ $ MouseUp_MoveSquare x                          
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
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
                        if state.isShowFlipCounts then
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
            -- Events bubble up from inner elements to outer elements, so inner event handlers will be run first
            mouseDown_MoveSquare <- H.gets _.mb_MouseDown_MoveSquare

            when (mouseDown_MoveSquare == Just x) do
                gameHistory <- H.gets _.gameHistory            
                let gameHistory' = unsafePartial fromRight $ applyMoveOnHistory rec.move gameHistory
                players <- H.gets _.players
                --let gameHistory'' = moveSequence players gameHistory'

                H.modify (_ 
                    { gameHistory = gameHistory'
                    , mb_SuggestedMove = mbSuggestedMove players $ gameStateOn gameHistory'
                    }
                )

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

        Click_GameStartStop next -> do
            H.modify (_ 
                { isGameStarted = true
                , isImminentGameStart = false
                }
            )       
            
            players <- H.gets _.players  
            gameHistory <- H.gets _.gameHistory  
            let gameState = gameStateOn gameHistory
            when ((not $ isStartGameState gameState) && (not $ isComputerVsComputer players)) do
                H.modify (_ 
                    { isAwaitingNewGameConfirm = true
                    }
                )   

            pure next

        Click_FlipCounts next -> do
            isShowFlipCounts <- H.gets _.isShowFlipCounts  
            H.modify (_ 
                { isShowFlipCounts = not isShowFlipCounts
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
            let player' = Player DFLT.defaultPlayerType_Computer  
            let players' = setItemColored color players player'

            H.modify (_ 
                { players = players' 
                }
            )  
            pure next

        Click_Settings_Person color next -> do     
            players <- H.gets _.players 
            let player' = Player DFLT.defaultPlayerType_Person            
            let players' = setItemColored color players player'

            H.modify (_ 
                { players = players'
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
                        , mb_SuggestedMove = mbSuggestedMove players $ gameStateOn gameHistory'
                        }
                    )
                    pure next

        Click_Close_NewGameConfirm next -> do
            H.modify (_ 
                { isAwaitingNewGameConfirm = false
                }
            )         
            pure next

        Click_NewGame next -> do 
            H.put initialState
            pure next

        NullOp next -> do
            pure next
