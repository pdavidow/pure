module StatusStartRestart
    ( Status_StartRestart(..) )

    where

import Prelude


data Status_StartRestart 
    = NotStarted
    | Started 
    | AwaitingRestart      

derive instance eqStartRestart :: Eq Status_StartRestart    