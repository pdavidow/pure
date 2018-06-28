module BoardComponent
    ( Query
    , component
    )
    where

import Prelude
import Board (boardElems, movePosition)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Either (fromRight)
import Data.List (List(Nil), elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, isJust)
import Disk (Color(..), toggleColor)
import Display (Move_DisplaySquare(..), FilledSelf_DisplaySquare(..), FilledOpponent_DisplaySquare(..), Tagged_DisplaySquare(..), toDisplaySquare, toPosition)
import DisplayConstants as DC
import GameHistory (GameHistory, applyMoveOnHistory, makeHistory, undoHistoryOnce)
import GameState (NextMoves, Tagged_GameState, board_FromTaggedGameState, nextMoves_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Position (Position)


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
    , focused_MoveSquare :: Maybe Move_DisplaySquare
    , mouseDown_MoveSquare :: Maybe Move_DisplaySquare
    , moves_FocusedFilledOpponentSquare :: List Position   
    , outflanks_FocusedMoveSquare :: List Position 
    , outflanks_FocusedFilledOpponentSquare :: List Position
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
        , focused_MoveSquare: Nothing
        , mouseDown_MoveSquare: Nothing
        , outflanks_FocusedMoveSquare: Nil
        , moves_FocusedFilledOpponentSquare: Nil        
        , outflanks_FocusedFilledOpponentSquare: Nil
        }
  

    isUndoable :: GameHistory -> Boolean
    isUndoable history =
        isJust $ undoHistoryOnce history


    render :: State -> H.ComponentHTML Query
    render state =
        -- https://www.w3schools.com/css/css_grid.asp
        -- https://css-tricks.com/snippets/css/complete-guide-grid/
        -- https://github.com/tachyons-css/tachyons/issues/372
        -- https://tachyons-css.slack.com/archives/C2W7UNRMJ/p1529697068000052
        HH.div
            [ HE.onMouseUp $ HE.input_ $ MouseUp_Anywhere ]
            [ HH.button
                [ HP.classes [ HH.ClassName "bg-red ba bw1 b--black f3 lh-copy" ]
                , HP.title "UNDO"
                , HP.enabled $ isUndoable state.gameHistory
                , HE.onClick (HE.input_ Undo)
                ]
                [ HH.text "UNDO" ]
            , HH.div
                [ HP.classes [ HH.ClassName "grid-container" ] ] 
                ( map renderSquare squares )
            ]
        where 


        moveColor :: Color
        moveColor =
            unsafePartial fromJust $ mbNextMoveColor_FromTaggedGameState gameState


        gameState :: Tagged_GameState
        gameState = 
            NE.last state.gameHistory


        squares :: Array Tagged_DisplaySquare
        squares =
            (boardElems $ board_FromTaggedGameState gameState)
                # map (toDisplaySquare moveColor moves)    


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
                            [ HH.ClassName $ DC.fillableGridItem <> 
                                if isMove_FocusedMoveSquare x then  
                                    DC.moveSquareColor_FocusedMoveSquare <> 
                                    DC.moveSquareBorder_FocusedMoveSquare                            
                                else if isMove_FocusedFilledOpponentSquare x then 
                                    DC.moveSquareColor_FocusedFilledOpponentSquare <> 
                                    DC.moveSquareBorder_FocusedFilledOpponentSquare
                                else
                                    DC.moveSquareColor <> 
                                    DC.squareBorder_Default
                            ]                       
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


            isMove_FocusedMoveSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedMoveSquare moveSquare =
                Just moveSquare == state.focused_MoveSquare 


            isMove_MouseDownMoveSquare :: Move_DisplaySquare -> Boolean
            isMove_MouseDownMoveSquare moveSquare =
                Just moveSquare == state.mouseDown_MoveSquare


            isMove_FocusedFilledOpponentSquare :: Move_DisplaySquare -> Boolean
            isMove_FocusedFilledOpponentSquare (Move_DisplaySquare rec) =
                elem (movePosition rec.move) state.moves_FocusedFilledOpponentSquare


            isOutflankSquare_FocusedMoveSquare :: Boolean
            isOutflankSquare_FocusedMoveSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare


            isOutflankSquare_MouseDownMoveSquare :: Boolean
            isOutflankSquare_MouseDownMoveSquare =
                isOutflankSquare_FocusedMoveSquare && isJust state.mouseDown_MoveSquare


            isOutflankSquare_FocusedFilledOpponentSquare :: Boolean
            isOutflankSquare_FocusedFilledOpponentSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare


            diskClasses :: String
            diskClasses = 
                case taggedDisplaySquare of 
                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        ""

                    Tagged_Move_DisplaySquare x ->           
                        -- if isMove_FocusedMoveSquare x || isMove_FocusedFilledOpponentSquare x then
                        if isMove_FocusedMoveSquare x && isMove_MouseDownMoveSquare x then
                            potentialDiskClassesForColor moveColor
                        else
                            ""                            

                    Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)  -> 
                        placedDiskClassesForColor rec.color

                    Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)  -> 
                        if isOutflankSquare_MouseDownMoveSquare then
                            flipDiskClassesForColor $ toggleColor rec.color
                        else
                            placedDiskClassesForColor $ rec.color 


            potentialDiskClassesForColor :: Color -> String
            potentialDiskClassesForColor color =
                case color of
                    Black -> DC.potentialDisk_Black 
                    White -> DC.potentialDisk_White


            flipDiskClassesForColor :: Color -> String
            flipDiskClassesForColor color =
                case color of
                    Black -> DC.flipDisk_Black  
                    White -> DC.flipDisk_White


            placedDiskClassesForColor :: Color -> String
            placedDiskClassesForColor color =
                case color of
                    Black -> DC.placedDisk_Black
                    White -> DC.placedDisk_White


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of    
        MouseEnter_MoveSquare x@(Move_DisplaySquare rec) next -> do
            H.modify (_ 
                { outflanks_FocusedMoveSquare = rec.outflanks
                , focused_MoveSquare = Just x 
                }
            )
            pure next

        MouseLeave_MoveSquare next -> do
            H.modify (_ 
                { outflanks_FocusedMoveSquare = Nil
                , focused_MoveSquare = Nothing
                }
            )
            pure next

        MouseDown_MoveSquare x next -> do
            H.modify (_ 
                { mouseDown_MoveSquare = Just x
                }
            )
            pure next

        MouseUp_Anywhere next -> do
            H.modify (_ 
                { focused_MoveSquare = Nothing
                , mouseDown_MoveSquare = Nothing                
                , outflanks_FocusedMoveSquare = Nil
                }
            )    
            pure next

        MouseUp_MoveSquare x@(Move_DisplaySquare rec) next -> do
            mouseDown_MoveSquare <- H.gets _.mouseDown_MoveSquare 

            when (mouseDown_MoveSquare == Just x) do
                history <- H.gets _.gameHistory
                let history' = unsafePartial fromRight $ applyMoveOnHistory rec.move history
                H.modify (_ { gameHistory = history' })

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

           
