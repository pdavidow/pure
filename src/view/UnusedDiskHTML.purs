module UnusedDiskHTML
    ( unusedDisk_HTML
    )
    where

import Prelude

import BlackWhite (getItemBlack, getItemWhite)
import Disk (Color(..))
import Display (unusedDiskClassesForColor)
import GameState (unusedDiskCounts_FromTaggedGameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Helper as HLPR
import Lib (haskellRange)
import Query (Query)
import State (State)
import Type.Data.Boolean (kind Boolean)
import UnusedDiskCount (UnusedDiskCounts, maxDiskCount)
import ViewLib (setCssProp)


unusedDisk_HTML :: State -> H.ComponentHTML Query  
unusedDisk_HTML state =
    HH.div                
        [ HP.classes [ HH.ClassName "unusedDiskGrids-grid" ]
        , setCssProp "--maxUnusedDiskCount" $ show maxDiskCount 
        ]  
        [ HH.div            
            [ HP.classes [ HH.ClassName "unusedDisk-grid" ]
            ]
            ( map (const $ renderUnusedDisk Black) $ haskellRange 1 $ getItemBlack $ unusedDiskCounts state) -- todo use repeat ?
        , HH.div            
            [ HP.classes [ HH.ClassName "unusedDisk-grid" ] 
            ]                
            ( map (const $ renderUnusedDisk White) $ haskellRange 1 $ getItemWhite $ unusedDiskCounts state) -- todo use repeat ? 
        ] 


renderUnusedDisk :: Color -> H.ComponentHTML Query
renderUnusedDisk color =
    HH.figure
        [ HP.classes [ HH.ClassName $ unusedDiskClassesForColor color] ] 
        []      


unusedDiskCounts :: State -> UnusedDiskCounts
unusedDiskCounts state =
    unusedDiskCounts_FromTaggedGameState $ HLPR.gameStateOn state                