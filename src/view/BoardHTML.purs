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
import DisplaySquare as DSQ
import ClassConstants as CC
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


squares :: State -> Array DSQ.Tagged_DisplaySquare
squares state =
    (boardElems $ board_FromTaggedGameState gameState)
        # map (DSQ.toDisplaySquare gameState $ HLPR.isGameStarted state)   
        where
            gameState = HLPR.gameStateOn state


renderSquare :: State -> DSQ.Tagged_DisplaySquare -> H.ComponentHTML Query
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
        DSQ.Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
            [ HP.classes 
                [ HH.ClassName $ CC.basicGridItem <> 
                    CC.defaultSquareColor <> 
                    CC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent 
            ]                    

        DSQ.Tagged_Filled_NotStartedGame_DisplaySquare _ -> 
            [ HP.classes 
                [ HH.ClassName $ CC.fillableGridItem <> 
                    CC.defaultSquareColor <> 
                    CC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]                     

        DSQ.Tagged_Empty_NonMove_DisplaySquare _ ->
            [ HP.classes 
                [ HH.ClassName $ CC.basicGridItem <> 
                    CC.defaultSquareColor <> 
                    CC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]

        DSQ.Tagged_Move_DisplaySquare x ->
            [ HP.classes 
                [ HH.ClassName $ CC.fillableGridItem <> squareProps__Move_DisplaySquare state x ]                       
            , HE.onMouseEnter $ HE.input_ $ MouseEnter_MoveSquare x
            , HE.onMouseLeave $ HE.input_ $ MouseLeave_MoveSquare                      
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent 
            ]
            <> guard (isPlayer_Person $ unsafe_CurrentPlayer (HLPR.sequenceStateRecOn state).players $ HLPR.gameStateOn state) 
            [ HE.onMouseDown $ HE.input_ $ MouseDown_MoveSquare x
            , HE.onMouseUp $ HE.input_ $ MouseUp_MoveSquare x                                
            ] 

        DSQ.Tagged_FilledSelf_DisplaySquare (DSQ.FilledSelf_DisplaySquare rec) ->
            [ HP.classes 
                [ HH.ClassName $ CC.fillableGridItem <> 
                    CC.defaultSquareColor <> 
                    borderProps_Filled rec.isPriorMove rec.isOutflankOfPriorMove
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]                    

        DSQ.Tagged_FilledOpponent_DisplaySquare x@(DSQ.FilledOpponent_DisplaySquare rec) ->
            [ HP.classes 
                [ HH.ClassName $ CC.fillableGridItem <> 
                    (   if DSP.isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare then 
                            CC.outflankSquareColor_FocusedMoveSquare 
                        else if DSP.isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare then 
                            CC.outflankSquareColor_FocusedFilledOpponentSquare                                  
                        else 
                            CC.defaultSquareColor                             
                    )
                    <> borderProps_Filled rec.isPriorMove rec.isOutflankOfPriorMove
                ]
            , HE.onMouseEnter $ HE.input_ $ MouseEnter_FilledOpponentSquare x
            , HE.onMouseLeave $ HE.input_ $ MouseLeave_FilledOpponentSquare                          
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ] 

        DSQ.Tagged_Empty_EndedGame_DisplaySquare _ ->
            [ HP.classes 
                [ HH.ClassName $ CC.basicGridItem <> 
                    CC.defaultSquareColor <> 
                    CC.squareBorder_Default
                ]
            , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
            ]

        DSQ.Tagged_Filled_EndedGame_DisplaySquare (DSQ.Filled_EndedGame_DisplaySquare rec) ->
            let
                squareColor = 
                    case rec.mbIsWinningColor of
                        Nothing -> CC.tieEndedSquareColor 
                        
                        Just boolean ->
                            case boolean of
                                true  -> CC.winEndedSquareColor 
                                false -> CC.defaultSquareColor
            in
                [ HP.classes 
                    [ HH.ClassName $ CC.fillableGridItem <> 
                        squareColor <> 
                        borderProps_Filled rec.isPriorMove rec.isOutflankOfPriorMove
                    ]
                , HE.onDragStart $ HE.input $ PreventDefault <<< toEvent
                ] 


squareProps__Move_DisplaySquare :: State -> DSQ.Move_DisplaySquare -> String
squareProps__Move_DisplaySquare state moveSquare =
    backgroundColorProps <> borderProps
    where
        backgroundColorProps =
            if DSP.isMove_FocusedMoveSquare state moveSquare then  
                CC.moveSquareColor_FocusedMoveSquare 
            else if DSP.isMove_FocusedFilledOpponentSquare state moveSquare then  
                CC.moveSquareColor_FocusedFilledOpponentSquare
            else
                CC.moveSquareColor                     

        borderProps = 
            if DSP.isSuggestedMoveSquare state moveSquare then
                CC.moveSquareBorder_Suggested
            else 
                CC.moveSquareBorder_NonSuggested


borderProps_Filled :: Boolean -> Boolean -> String
borderProps_Filled isPriorMove isOutflankOfPriorMove =
    if isPriorMove then 
        CC.filledSquareBorder_PriorMove 
    else if isOutflankOfPriorMove then 
        CC.filledSquareBorder_OutflankOfPriorMove
    else 
        CC.squareBorder_Default                