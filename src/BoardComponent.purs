module BoardComponent
    ( Query
    , component
    )
    where

import Prelude
import Board (Move, boardElems, movePosition)
import BoardSize (boardSize)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Either (fromRight)
import Data.List (List(Nil), elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, isJust)
import Disk (Color(..), toggleColor)
import Display (Move_DisplaySquare(..), FilledSelf_DisplaySquare(..), FilledOpponent_DisplaySquare(..), Empty_EndedGame_DisplaySquare(..), Filled_EndedGame_DisplaySquare(..), Tagged_DisplaySquare(..), toDisplaySquare, toPosition, status)
import DisplayConstants as DC
import GameHistory (GameHistory, applyMoveOnHistory, makeHistory, undoHistoryOnce)
import GameState (NextMoves, Tagged_GameState, board_FromTaggedGameState, nextMoves_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState, unusedDiskCounts_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib (setCssProp, haskellRange)
import Partial.Unsafe (unsafePartial)
import Position (Position)
import Search (SearchDepth(..), mbBestNextMove, defaultSearchDepth)
import Type.Data.Boolean (kind Boolean)
import UnusedDiskCount (UnusedDiskCounts, maxDiskCount, black, white)


data Query a
  = MouseEnter_MoveSquare Move_DisplaySquare a
  | MouseLeave_MoveSquare a
  | MouseDown_MoveSquare Move_DisplaySquare a
  | MouseUp_Anywhere a
  | MouseUp_MoveSquare Move_DisplaySquare a  
  | MouseEnter_FilledOpponentSquare FilledOpponent_DisplaySquare a
  | MouseLeave_FilledOpponentSquare a  
  | PreventDefault Event a
  | Undo a

type State = 
    { gameHistory :: GameHistory
    , mb_focused_MoveSquare :: Maybe Move_DisplaySquare
    , mb_mouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
    , searchDepth :: SearchDepth
    , mb_suggestedMove :: Maybe Move
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
        { gameHistory: makeHistory
        , mb_focused_MoveSquare: Nothing
        , mb_mouseDown_MoveSquare: Nothing
        , outflanks_FocusedMoveSquare: Nil
        , moves_FocusedFilledOpponentSquare: Nil        
        , outflanks_FocusedFilledOpponentSquare: Nil
        , searchDepth: defaultSearchDepth
        , mb_suggestedMove: Nothing
        }
  

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
        -- https://www.w3schools.com/css/css_grid.asp
        -- https://css-tricks.com/snippets/css/complete-guide-grid/
        -- https://github.com/tachyons-css/tachyons/issues/372
        -- https://tachyons-css.slack.com/archives/C2W7UNRMJ/p1529697068000052
        -- https://gridbyexample.com/

        HH.div
            [ HE.onMouseUp $ HE.input_ $ MouseUp_Anywhere 
            ]
            [ HH.div
                [ HP.classes [ HH.ClassName "board-grid" ]
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
                    ( map (const $ renderUnusedDisk Black) $ haskellRange 1 $ black unusedDiskCounts) -- todo use repeat ?
                , HH.div            
                    [ HP.classes [ HH.ClassName "unusedDisk-grid" ] 
                    ]                
                    ( map (const $ renderUnusedDisk White) $ haskellRange 1 $ white unusedDiskCounts) -- todo use repeat ? 
                ] 
            , HH.button
                [ HP.classes [ HH.ClassName "mt4 ml4" ]
                , HP.enabled $ isUndoable state.gameHistory
                , HE.onClick (HE.input_ Undo)
                ]
                [ HH.text "Undo" ] 
            , HH.div
                [ HP.classes [ HH.ClassName "mt4 ml4 f3 lh-copy b" ]
                ]
                [ HH.text $ "STATUS: " <> status gameState ]                 
            ]
        where 

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
                # map (toDisplaySquare gameState)  


        moves :: NextMoves
        moves = 
            nextMoves_FromTaggedGameState gameState


        renderSquare :: Tagged_DisplaySquare -> H.ComponentHTML Query
        renderSquare taggedDisplaySquare =
            HH.figure
                squareProps
                [ HH.div
                    [ HP.classes [ HH.ClassName diskClasses ] ]
                    []
                ] 
            where

            squareProps =
                case taggedDisplaySquare of
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

                    Tagged_Empty_EndedGame_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.basicGridItem <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_Filled_EndedGame_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ DC.fillableGridItem <> 
                                DC.defaultSquareColor <> 
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
                case state.mb_suggestedMove of
                    Nothing -> false
                    Just move -> move == rec.move


            isMove_FocusedMoveSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedMoveSquare moveSquare =
                Just moveSquare == state.mb_focused_MoveSquare 


            isMove_FocusedFilledOpponentSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedFilledOpponentSquare (Move_DisplaySquare rec) =
                elem (movePosition rec.move) state.moves_FocusedFilledOpponentSquare


            isOutflankSquare_FocusedMoveSquare :: Boolean
            isOutflankSquare_FocusedMoveSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare


            isOutflankSquare_MouseDownMoveSquare :: Boolean
            isOutflankSquare_MouseDownMoveSquare =
                isOutflankSquare_FocusedMoveSquare && isJust state.mb_mouseDown_MoveSquare


            isOutflankSquare_FocusedFilledOpponentSquare :: Boolean
            isOutflankSquare_FocusedFilledOpponentSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare


            diskClasses :: String
            diskClasses = 
                case taggedDisplaySquare of 
                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        ""

                    Tagged_Move_DisplaySquare x ->           
                        if isMove_FocusedMoveSquare x && isJust state.mb_mouseDown_MoveSquare then
                            DC.potentialDiskClassesForColor $ unsafePartial fromJust $ mbMoveColor
                        else
                            ""                            

                    Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)  -> 
                        DC.placedDiskClassesForColor rec.color

                    Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)  -> 
                        if isOutflankSquare_MouseDownMoveSquare then
                            DC.flipDiskClassesForColor $ toggleColor rec.color
                        else
                            DC.placedDiskClassesForColor $ rec.color 

                    Tagged_Empty_EndedGame_DisplaySquare _ ->       
                        ""                            
                        
                    Tagged_Filled_EndedGame_DisplaySquare (Filled_EndedGame_DisplaySquare rec) -> 
                        DC.placedDiskClassesForColor rec.color                    


        renderUnusedDisk :: Color -> H.ComponentHTML Query
        renderUnusedDisk color =
            HH.figure
                [ HP.classes [ HH.ClassName $ DC.unusedDiskClassesForColor color] ] 
                []


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of    
        MouseEnter_MoveSquare x@(Move_DisplaySquare rec) next -> do
            H.modify (_ 
                { outflanks_FocusedMoveSquare = rec.outflanks
                , mb_focused_MoveSquare = Just x 
                }
            )
            pure next

        MouseLeave_MoveSquare next -> do
            H.modify (_ 
                { outflanks_FocusedMoveSquare = Nil
                , mb_focused_MoveSquare = Nothing
                }
            )
            pure next

        MouseDown_MoveSquare x next -> do
            H.modify (_ 
                { mb_mouseDown_MoveSquare = Just x
                }
            )
            pure next

        MouseUp_MoveSquare x@(Move_DisplaySquare rec) next -> do
            -- Events bubble from inner elements to outer elements, so inner event handlers will be run first
            mouseDown_MoveSquare <- H.gets _.mb_mouseDown_MoveSquare

            when (mouseDown_MoveSquare == Just x) do
                history <- H.gets _.gameHistory            
                let history' = unsafePartial fromRight $ applyMoveOnHistory rec.move history
                let gameState' = gameStateOn history'
                searchDepth <- H.gets _.searchDepth

                H.modify (_ 
                    { gameHistory = history'
                    --, mb_suggestedMove = mbBestNextMove searchDepth gameState'
                    }
                )

            pure next       

        MouseUp_Anywhere next -> do
            H.modify (_ 
                { mb_focused_MoveSquare = Nothing
                , mb_mouseDown_MoveSquare = Nothing                
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

        PreventDefault event next -> do
            H.liftEff $ preventDefault event
            pure next

        Undo next -> do
            history <- H.gets _.gameHistory
            let mbHistory = undoHistoryOnce history

            case mbHistory of
                Nothing -> do
                    pure next

                Just history -> do
                    H.modify (_ { gameHistory = history})
                    pure next

           
