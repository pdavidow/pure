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
  = Click_MoveSquare Move_DisplaySquare a
  | MouseEnter_MoveSquare Move_DisplaySquare a
  | MouseLeave_MoveSquare a
  | MouseEnter_FilledOpponentSquare FilledOpponent_DisplaySquare a
  | MouseLeave_FilledOpponentSquare a  
  | PreventDefault Event a
  | Undo a

type State = 
    { gameHistory :: GameHistory
    , inFocusMoveSquare :: Maybe Move_DisplaySquare
    , inFocusFilledOpponentSquare :: Maybe FilledOpponent_DisplaySquare
    , moves_InFocusFilledOpponentSquare :: List Position   
    , outflanks_InFocusMoveSquare :: List Position 
    , outflanks_InFocusFilledOpponentSquare :: List Position
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
        , inFocusMoveSquare: Nothing
        , inFocusFilledOpponentSquare: Nothing
        , outflanks_InFocusMoveSquare: Nil
        , moves_InFocusFilledOpponentSquare: Nil        
        , outflanks_InFocusFilledOpponentSquare: Nil
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
        HH.div_
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
                            [ HH.ClassName $ "grid-item h3 w3 " <> 
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_Move_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ "grid-item h3 w3 flex justify-center "<> 
                                if isMove_InFocusMoveSquare x then  
                                    DC.moveSquareColor_InFocusMoveSquare <> 
                                    DC.moveSquareBorder_InFocusMoveSquare                            
                                else if isMove_InFocusFilledOpponentSquare x then 
                                    DC.moveSquareColor_InFocusFilledOpponentSquare <> 
                                    DC.moveSquareBorder_InFocusFilledOpponentSquare
                                else
                                    DC.moveSquareColor <> 
                                    DC.squareBorder_Default
                            ] 
                        , HE.onClick $ HE.input_ $ Click_MoveSquare x
                        , HE.onMouseEnter $ HE.input_ $ MouseEnter_MoveSquare x
                        , HE.onMouseLeave $ HE.input_ $ MouseLeave_MoveSquare
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_FilledSelf_DisplaySquare _ ->
                        [ HP.classes 
                            [ HH.ClassName $ "grid-item h3 w3 flex justify-center "  <>
                                DC.defaultSquareColor <> 
                                DC.squareBorder_Default
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]                    
            
                    Tagged_FilledOpponent_DisplaySquare x ->
                        [ HP.classes 
                            [ HH.ClassName $ "grid-item h3 w3 flex justify-center "  <>
                                if isOutflankSquare_InFocusMoveSquare then 
                                    DC.outflankSquareColor_InFocusMoveSquare <> 
                                    DC.outflankSquareBorder_InFocusMoveSquare
                                else if isOutflankSquare_InFocusFilledOpponentSquare then 
                                    DC.outflankSquareColor_InFocusFilledOpponentSquare <> 
                                    DC.outflankSquareBorder_InFocusFilledOpponentSquare                                    
                                else 
                                    DC.defaultSquareColor <> 
                                    DC.squareBorder_Default
                            ]
                        , HE.onMouseEnter $ HE.input_ $ MouseEnter_FilledOpponentSquare x
                        , HE.onMouseLeave $ HE.input_ $ MouseLeave_FilledOpponentSquare                          
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ] 


            isMove_InFocusMoveSquare :: Move_DisplaySquare -> Boolean
            isMove_InFocusMoveSquare moveSquare =
                Just moveSquare == state.inFocusMoveSquare 


            isMove_InFocusFilledOpponentSquare :: Move_DisplaySquare -> Boolean
            isMove_InFocusFilledOpponentSquare (Move_DisplaySquare rec) =
                elem (movePosition rec.move) state.moves_InFocusFilledOpponentSquare


            isOutflankSquare_InFocusMoveSquare :: Boolean
            isOutflankSquare_InFocusMoveSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_InFocusMoveSquare


            isOutflankSquare_InFocusFilledOpponentSquare :: Boolean
            isOutflankSquare_InFocusFilledOpponentSquare =
                elem (toPosition taggedDisplaySquare) state.outflanks_InFocusFilledOpponentSquare


            diskClasses :: String
            diskClasses = 
                case taggedDisplaySquare of 
                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        ""

                    Tagged_Move_DisplaySquare x ->           
                        -- if isMove_InFocusMoveSquare x || isMove_InFocusFilledOpponentSquare x then
                        if isMove_InFocusMoveSquare x then
                            potentialDiskClassesForColor moveColor
                        else
                            ""                            

                    Tagged_FilledSelf_DisplaySquare (FilledSelf_DisplaySquare rec)  -> 
                        placedDiskClassesForColor rec.color

                    Tagged_FilledOpponent_DisplaySquare (FilledOpponent_DisplaySquare rec)  -> 
                        placedDiskClassesForColor $
                            -- if isOutflankSquare_InFocusMoveSquare || isOutflankSquare_InFocusFilledOpponentSquare then
                            if isOutflankSquare_InFocusMoveSquare then
                                toggleColor rec.color
                            else
                                rec.color


            potentialDiskClassesForColor :: Color -> String
            potentialDiskClassesForColor color =
                case color of
                    Black -> DC.potentialDisk_Black 
                    White -> DC.potentialDisk_White


            placedDiskClassesForColor :: Color -> String
            placedDiskClassesForColor color =
                case color of
                    Black -> DC.placedDisk_Black
                    White -> DC.placedDisk_White


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of        
        Click_MoveSquare (Move_DisplaySquare rec) next -> do
            history <- H.gets _.gameHistory
            let history' = unsafePartial fromRight $ applyMoveOnHistory rec.move history
            H.modify (_ 
                { gameHistory = history'
                , outflanks_InFocusMoveSquare = Nil
                }
            )
            pure next
            
        MouseEnter_MoveSquare x@(Move_DisplaySquare rec) next -> do
            H.modify (_ 
                { outflanks_InFocusMoveSquare = rec.outflanks
                , inFocusMoveSquare = Just x 
                }
            )
            pure next

        MouseLeave_MoveSquare next -> do
            H.modify (_ 
                { outflanks_InFocusMoveSquare = Nil
                , inFocusMoveSquare = Nothing
                }
            )
            pure next
            
        MouseEnter_FilledOpponentSquare (FilledOpponent_DisplaySquare rec) next -> do
            H.modify (_ 
                { outflanks_InFocusFilledOpponentSquare = rec.outflanks
                , moves_InFocusFilledOpponentSquare = rec.moves
                }
            )
            pure next

        MouseLeave_FilledOpponentSquare next -> do
            H.modify (_ 
                { outflanks_InFocusFilledOpponentSquare = Nil
                , moves_InFocusFilledOpponentSquare = Nil
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

           
