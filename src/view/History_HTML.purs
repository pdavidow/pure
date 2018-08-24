module HistoryHTML
    ( history_HTML
    )

    where

import Prelude

import Board (boardElems)
import BoardSize (boardSize)
import ClassConstants as CC
import DOM.Classy.Event (toEvent)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import DiskHTML (diskClasses, diskChildren)
import Display as DSP
import DisplaySquare as DSQ
import GameState (Tagged_GameState)
import GameState (Tagged_GameState, board_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Player (isPlayer_Person)
import Query (Query(..))
import SequenceState (SequenceState(..))
import Sequencer (unsafe_CurrentPlayer)
import State (State)
import Type.Data.Boolean (kind Boolean)
import ViewLib (setCssProp)
import Data.List.NonEmpty as NE

history_HTML :: State -> H.ComponentHTML Query 
history_HTML state =
    HH.section
        [ HP.classes [ HH.ClassName "" ] 
        ] 
        ( map boardThumbnail $ NE.toUnfoldable state.history )     


boardThumbnail :: SequenceState -> H.ComponentHTML Query 
boardThumbnail sequenceState =
    HH.div
        [ HP.classes [ HH.ClassName "mt2 board-grid" ] 
        , setCssProp "--boardSize" $ show boardSize
        ] 
        ( map (renderSquare state) $ squares state )          