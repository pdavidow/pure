module Settings
    ( SettingsRec
    , EditPlayer(..)
    , EditPlayerType(..)
    , EditPlayerTypeRec
    , EditPlayers
    , defaultSettingsRec
    , settingsRecOn
    , toEditPlayers
    , toPlayers 
    )
    where 

import Prelude

import BlackWhite (BlackWhite(..), makeBlackWhite)
import Disk (Color(..))
import Player as PLYR
import PlayerDefaults as DFLT
import Search (SearchDepth)

type SettingsRec =   
    { players :: EditPlayers
    , selectedColor :: Color
    }

data EditPlayer = EditPlayer Color EditPlayerTypeRec

data EditPlayerType = EditComputer | EditPerson 

type EditPlayerTypeRec =         
    { playerType :: EditPlayerType
    , computer_searchDepth :: SearchDepth
    , computer_isRandomPick :: Boolean
    , person_searchDepth :: SearchDepth
    , person_isAutoSuggest :: Boolean
    } 
         
type EditPlayers = BlackWhite EditPlayer    

derive instance eqEditPlayerType :: Eq EditPlayerType    
derive instance eqEditPlayer :: Eq EditPlayer  
 

defaultSettingsRec :: SettingsRec
defaultSettingsRec =
    settingsRecOn DFLT.defaultPlayers


settingsRecOn :: PLYR.Players -> SettingsRec
settingsRecOn players = 
    { selectedColor: Black -- arbitary
    , players: toEditPlayers players
    }


defaultEditPlayerTypeRec :: EditPlayerTypeRec
defaultEditPlayerTypeRec =
    { playerType: EditPerson -- whatever
    , computer_searchDepth: DFLT.defaultComputer_searchDepth
    , computer_isRandomPick: DFLT.defaultComputer_isRandomPick                        
    , person_searchDepth: DFLT.defaultPerson_searchDepth
    , person_isAutoSuggest: DFLT.defaultPerson_isAutoSuggest
    }  


toEditPlayer :: PLYR.Player -> EditPlayer
toEditPlayer (PLYR.Player color playerType) =
    EditPlayer color x
    where 
        x = 
            case playerType of 
                PLYR.Computer rec ->
                    defaultEditPlayerTypeRec
                        { playerType = EditComputer
                        , computer_searchDepth = rec.searchDepth
                        , computer_isRandomPick = rec.isRandomPick
                        }  

                PLYR.Person rec -> 
                    defaultEditPlayerTypeRec
                        { playerType = EditPerson
                        , person_searchDepth = rec.searchDepth
                        , person_isAutoSuggest = rec.isAutoSuggest
                        }                 


toPlayer ::  EditPlayer -> PLYR.Player
toPlayer (EditPlayer color rec) =
    PLYR.Player color $
        case rec.playerType of
            EditComputer ->
                PLYR.Computer
                    { searchDepth: rec.computer_searchDepth
                    , isRandomPick: rec.computer_isRandomPick
                    }  

            EditPerson -> 
                PLYR.Person 
                    { searchDepth: rec.person_searchDepth
                    , isAutoSuggest: rec.person_isAutoSuggest
                    } 


toEditPlayers :: PLYR.Players -> EditPlayers
toEditPlayers (BlackWhite {black: b, white: w}) =
    makeBlackWhite 
        (toEditPlayer b) 
        (toEditPlayer w)           


toPlayers :: EditPlayers -> PLYR.Players
toPlayers (BlackWhite {black: b, white: w}) =
    makeBlackWhite (toPlayer b) (toPlayer w)    

