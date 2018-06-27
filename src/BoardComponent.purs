module BoardComponent
    ( Query
    , component
    )
    where

import Prelude
import Board (boardElems)
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Either (fromRight)
import Data.List (List(Nil), elem)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, isJust)
import Disk (Color(..), toggleColor)
import DisplayConstants (defaultSquareColor, moveSquareColor, outflankSquareColor, defaultSquareBorder, outflankSquareBorder, blackDisk, whiteDisk)
import GameHistory (GameHistory, applyMoveOnHistory, makeHistory, undoHistoryOnce)
import GameState (NextMoves, Tagged_GameState, board_FromTaggedGameState, nextMoves_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Position (Position)
import Display (Move_DisplaySquare(..), Filled_DisplaySquare(..), Tagged_DisplaySquare(..), toDisplaySquare, toPosition)


data Query a
  = Click_MoveSquare Move_DisplaySquare a
  | MouseEnter_MoveSquare Move_DisplaySquare a
  | MouseLeave_MoveSquare a
  | PreventDefault Event a
  | Undo a

type State = 
    { gameHistory :: GameHistory
    , currentMoveSquare :: Maybe Move_DisplaySquare
    , outflankPositions :: List Position
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
        , currentMoveSquare: Nothing
        , outflankPositions: Nil
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


        gameState :: Tagged_GameState
        gameState = 
            NE.last state.gameHistory


        squares :: Array Tagged_DisplaySquare
        squares =
            (boardElems $ board_FromTaggedGameState gameState)
                # map (toDisplaySquare nextMoves) 


        nextMoves :: NextMoves
        nextMoves = 
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
                        [ HP.classes [ HH.ClassName $ "grid-item h3 w3 "  <> defaultSquareColor <> defaultSquareBorder]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_Move_DisplaySquare x ->
                        [ HP.classes [ HH.ClassName $ "grid-item h3 w3 flex justify-center " <> moveSquareColor <> defaultSquareBorder] 
                        , HE.onClick $ HE.input_ $ Click_MoveSquare x
                        , HE.onMouseEnter $ HE.input_ $ MouseEnter_MoveSquare x
                        , HE.onMouseLeave $ HE.input_ $ MouseLeave_MoveSquare
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]

                    Tagged_Filled_DisplaySquare _ ->
                        [ HP.classes 
                            [ HH.ClassName $ "grid-item h3 w3 flex justify-center "  <>
                                if isOutflankSquare then 
                                    outflankSquareColor <> outflankSquareBorder
                                else 
                                    defaultSquareColor <> defaultSquareBorder
                            ]
                        , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                        ]                    
            

            isCurrentMoveSquare :: Move_DisplaySquare -> Boolean
            isCurrentMoveSquare moveSquare =
                Just moveSquare == state.currentMoveSquare 


            isOutflankSquare :: Boolean
            isOutflankSquare =
                elem (toPosition taggedDisplaySquare) state.outflankPositions


            currentMoveColor ::Color
            currentMoveColor =
                unsafePartial fromJust $ mbNextMoveColor_FromTaggedGameState gameState


            diskClasses :: String
            diskClasses = 
                case taggedDisplaySquare of 
                    Tagged_Empty_NonMove_DisplaySquare _ ->
                        ""
                    Tagged_Move_DisplaySquare x ->           
                        if isCurrentMoveSquare x then
                            diskClassesForColor currentMoveColor
                        else
                            ""                            

                    Tagged_Filled_DisplaySquare (Filled_DisplaySquare rec)  -> 
                        diskClassesForColor $
                            if isOutflankSquare then
                                toggleColor rec.color
                            else
                                rec.color


            diskClassesForColor :: Color -> String
            diskClassesForColor color =
                case color of
                    Black -> blackDisk
                    White -> whiteDisk


    eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
    eval = case _ of        
        Click_MoveSquare (Move_DisplaySquare rec) next -> do
            history <- H.gets _.gameHistory
            let history' = unsafePartial fromRight $ applyMoveOnHistory rec.move history
            H.modify (_ 
                { gameHistory = history'
                , outflankPositions = Nil
                }
            )
            pure next
            
        MouseEnter_MoveSquare x@(Move_DisplaySquare rec) next -> do
            H.modify (_ 
                { outflankPositions = rec.outflankPositions
                , currentMoveSquare = Just x 
                }
            )
            pure next

        MouseLeave_MoveSquare next -> do
            H.modify (_ 
                { outflankPositions = Nil
                , currentMoveSquare = Nothing
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

           
