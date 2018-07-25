module BoardHTML
    ( board_HTML
    )
    where

import Prelude

import Board (boardElems)
import BoardSize (boardSize)
import DOM.Classy.Event (toEvent)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import DiskHTML (diskClasses, diskChildren)
import Display as DSP
import DisplayConstants as DC
import GameState (board_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Player (isPlayer_Person)
import Query (Query(..))
import Sequencer (unsafe_CurrentPlayer)
import State (State)
import Type.Data.Boolean (kind Boolean)
import ViewLib (setCssProp)


board_HTML :: State -> H.ComponentHTML Query 
board_HTML state =
    HH.div
        [ HP.classes [ HH.ClassName "mt2 board-grid" ]
        , setCssProp "--boardSize" $ show boardSize
        ] 
        ( map (renderSquare state) $ squares state )               


squares :: State -> Array DSP.Tagged_DisplaySquare
squares state =
    (boardElems $ board_FromTaggedGameState $ HLPR.gameState state)
        # map (DSP.toDisplaySquare (HLPR.gameState state) $ HLPR.isGameStarted state)  


renderSquare :: State -> DSP.Tagged_DisplaySquare -> H.ComponentHTML Query
renderSquare state taggedDisplaySquare =
    HH.figure
        ( squareProps state taggedDisplaySquare )
        [ HH.div
            [ HP.classes [ HH.ClassName $ diskClasses state taggedDisplaySquare ] ]
            ( diskChildren state taggedDisplaySquare )
        ] 


squareProps :: _ -- todo
squareProps state taggedDisplaySquare =
    case taggedDisplaySquare of
        DSP.Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
            [ HP.classes 
                [ HH.ClassName $ DC.basicGridItem <> 
                    DC.defaultSquareColor <> 
                    DC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent 
            ]                    

        DSP.Tagged_Filled_NotStartedGame_DisplaySquare _ -> 
            [ HP.classes 
                [ HH.ClassName $ DC.fillableGridItem <> 
                    DC.defaultSquareColor <> 
                    DC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]                     

        DSP.Tagged_Empty_NonMove_DisplaySquare _ ->
            [ HP.classes 
                [ HH.ClassName $ DC.basicGridItem <> 
                    DC.defaultSquareColor <> 
                    DC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]

        DSP.Tagged_Move_DisplaySquare x ->
            [ HP.classes 
                [ HH.ClassName $ DC.fillableGridItem <> squareProps__Move_DisplaySquare state x ]                       
            , HE.onMouseEnter $ HE.input_ $ MouseEnter_MoveSquare x
            , HE.onMouseLeave $ HE.input_ $ MouseLeave_MoveSquare                      
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]
            <> guard (isPlayer_Person $ unsafe_CurrentPlayer state.players $ HLPR.gameState state) 
            [ HE.onMouseDown $ HE.input_ $ MouseDown_MoveSquare x
            , HE.onMouseUp $ HE.input_ $ MouseUp_MoveSquare x                                
            ] 

        DSP.Tagged_FilledSelf_DisplaySquare _ ->
            [ HP.classes 
                [ HH.ClassName $ DC.fillableGridItem <> 
                    DC.defaultSquareColor <> 
                    DC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]                    

        DSP.Tagged_FilledOpponent_DisplaySquare x ->
            [ HP.classes 
                [ HH.ClassName $ DC.fillableGridItem <> 
                    if HLPR.isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare then 
                        DC.outflankSquareColor_FocusedMoveSquare <> 
                        DC.outflankSquareBorder_FocusedMoveSquare
                    else if HLPR.isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare then 
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

        DSP.Tagged_Empty_EndedGame_DisplaySquare _ ->
            [ HP.classes 
                [ HH.ClassName $ DC.basicGridItem <> 
                    DC.defaultSquareColor <> 
                    DC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]

        DSP.Tagged_Filled_EndedGame_DisplaySquare (DSP.Filled_EndedGame_DisplaySquare rec) ->
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


squareProps__Move_DisplaySquare :: State -> DSP.Move_DisplaySquare -> String
squareProps__Move_DisplaySquare state moveSquare =
    backgroundColorProps <> borderProps
    where
        backgroundColorProps =
            if HLPR.isMove_FocusedMoveSquare state moveSquare then  
                DC.moveSquareColor_FocusedMoveSquare 
            else if HLPR.isMove_FocusedFilledOpponentSquare state moveSquare then  
                DC.moveSquareColor_FocusedFilledOpponentSquare
            else
                DC.moveSquareColor                    

        borderProps = 
            if HLPR.isSuggestedMoveSquare state moveSquare then
                DC.moveSquareBorder_Suggested
            else 
                DC.moveSquareBorder_NonSuggested
