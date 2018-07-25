module ViewLib
    ( setCssProp
    )
    where
      
import CSS as CSS      
import Halogen as H
import Halogen.HTML.CSS as HC      


setCssProp :: String -> String -> forall t1 t2. H.IProp ( style :: String | t1) t2
setCssProp key value =
    HC.style do (CSS.key (CSS.fromString key) value)      