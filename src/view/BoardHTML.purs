module BoardHTML
    ( board_HTML
    )
    where

import Prelude

import Board (boardElems, movePosition)
import BoardSize (boardSize)
import DOM.Classy.Event (toEvent)
import Data.List (elem)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Monoid (guard)
import Disk (toggleColor)
import Display as DSP
import DisplayConstants as DC
import GameState (board_FromTaggedGameState, mbNextMoveColor_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper (gameState, isGameStarted)
import Partial.Unsafe (unsafePartial)
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
    (boardElems $ board_FromTaggedGameState $ gameState state)
        # map (DSP.toDisplaySquare (gameState state) $ isGameStarted state)  


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
            <> guard (isPlayer_Person $ unsafe_CurrentPlayer state.players $ gameState state) 
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
                    if isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare then 
                        DC.outflankSquareColor_FocusedMoveSquare <> 
                        DC.outflankSquareBorder_FocusedMoveSquare
                    else if isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare then 
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
            if isMove_FocusedMoveSquare state moveSquare then  
                DC.moveSquareColor_FocusedMoveSquare 
            else if isMove_FocusedFilledOpponentSquare state moveSquare then  
                DC.moveSquareColor_FocusedFilledOpponentSquare
            else
                DC.moveSquareColor                    

        borderProps = 
            if isSuggestedMoveSquare state moveSquare then
                DC.moveSquareBorder_Suggested
            else 
                DC.moveSquareBorder_NonSuggested


isSuggestedMoveSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isSuggestedMoveSquare state (DSP.Move_DisplaySquare rec) =
    case state.mb_SuggestedMove of
        Nothing -> false
        Just move -> move == rec.move


isMove_FocusedMoveSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isMove_FocusedMoveSquare state moveSquare =
    Just moveSquare == state.mb_Focused_MoveSquare 


isMove_FocusedFilledOpponentSquare :: State -> DSP.Move_DisplaySquare -> Boolean
isMove_FocusedFilledOpponentSquare state (DSP.Move_DisplaySquare rec) =
    elem (movePosition rec.move) state.moves_FocusedFilledOpponentSquare


isOutflankSquare_FocusedMoveSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare =
    elem (DSP.toPosition taggedDisplaySquare) state.outflanks_FocusedMoveSquare


isOutflankSquare_MouseDownMoveSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_MouseDownMoveSquare state taggedDisplaySquare =
    (isOutflankSquare_FocusedMoveSquare state taggedDisplaySquare) && isJust state.mb_MouseDown_MoveSquare


isOutflankSquare_FocusedFilledOpponentSquare :: State -> DSP.Tagged_DisplaySquare -> Boolean
isOutflankSquare_FocusedFilledOpponentSquare state taggedDisplaySquare =
    elem (DSP.toPosition taggedDisplaySquare) state.outflanks_FocusedFilledOpponentSquare


diskClasses :: State -> DSP.Tagged_DisplaySquare -> String
diskClasses state taggedDisplaySquare =
    case taggedDisplaySquare of 
        DSP.Tagged_Empty_NotStartedGame_DisplaySquare _ -> 
            ""                 

        DSP.Tagged_Filled_NotStartedGame_DisplaySquare (DSP.Filled_NotStartedGame_DisplaySquare rec)  -> 
            if state.isImminentGameStart then
                DSP.placedDiskClassesForColor rec.color
            else
                ""

        DSP.Tagged_Empty_NonMove_DisplaySquare _ ->
            ""

        DSP.Tagged_Move_DisplaySquare x ->   
            let
                mbMoveColor = mbNextMoveColor_FromTaggedGameState $ gameState state
            in   
                if isMove_FocusedMoveSquare state x && isJust state.mb_MouseDown_MoveSquare then
                    DSP.potentialDiskClassesForColor $ unsafePartial fromJust $ mbMoveColor
                else
                    ""                            

        DSP.Tagged_FilledSelf_DisplaySquare (DSP.FilledSelf_DisplaySquare rec)  -> 
            DSP.placedDiskClassesForColor rec.color

        DSP.Tagged_FilledOpponent_DisplaySquare (DSP.FilledOpponent_DisplaySquare rec)  -> 
            if isOutflankSquare_MouseDownMoveSquare state taggedDisplaySquare then
                DSP.flipDiskClassesForColor $ toggleColor rec.color
            else
                DSP.placedDiskClassesForColor $ rec.color 

        DSP.Tagged_Empty_EndedGame_DisplaySquare _ ->       
            ""                            
            
        DSP.Tagged_Filled_EndedGame_DisplaySquare (DSP.Filled_EndedGame_DisplaySquare rec) -> 
            DSP.placedDiskClassesForColor rec.color                    


diskChildren :: State -> DSP.Tagged_DisplaySquare -> Array (H.ComponentHTML Query)
diskChildren state taggedDisplaySquare = 
    let
        mbFlipCount = 
            if state.isShow_FlipCounts then
                case taggedDisplaySquare of 
                    DSP.Tagged_Empty_NotStartedGame_DisplaySquare _                                -> Nothing
                    DSP.Tagged_Filled_NotStartedGame_DisplaySquare _                               -> Nothing
                    DSP.Tagged_Empty_NonMove_DisplaySquare _                                       -> Nothing
                    DSP.Tagged_Move_DisplaySquare _                                                -> Nothing                                    
                    DSP.Tagged_FilledSelf_DisplaySquare (DSP.FilledSelf_DisplaySquare rec)             -> Just rec.flipCount
                    DSP.Tagged_FilledOpponent_DisplaySquare (DSP.FilledOpponent_DisplaySquare rec)     -> Just rec.flipCount
                    DSP.Tagged_Empty_EndedGame_DisplaySquare _                                     -> Nothing                                                         
                    DSP.Tagged_Filled_EndedGame_DisplaySquare (DSP.Filled_EndedGame_DisplaySquare rec) -> Just rec.flipCount
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
