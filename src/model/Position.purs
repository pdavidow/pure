module Position 
     ( PositionRec
     , Position -- hiding constructor
     , PositionRow(..)
     , makeValidPosition
     , radiatingPositionRows
     , positionRec
     , adjacentPositions
     , isValidPositionRec
     )
     where

import Prelude

import BoardSize (boardSize)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.List (List(..), foldr, filter, fromFoldable, null, snoc)
import Lib (haskellRange, rights) 
import Partial.Unsafe (unsafeCrashWith)


type PositionRec = {x :: Int, y :: Int} -- one-based

newtype Position = Position PositionRec

newtype PositionRow = PositionRow (List Position)

data Dir = Inc | Dec


derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position
derive instance eqPositionRow :: Eq PositionRow

instance showPosition :: Show Position where
    show (Position ({x: i, y: j})) = 
        "Position {x: " <> show i <> ", y: " <> show j <> "}"


instance showPositionRow :: Show PositionRow where
    show (PositionRow list) =
        "PositionRow> " <> (foldr append "" $ map (\p -> show p <> " ") list)


isValidPositionRec :: PositionRec -> Boolean
isValidPositionRec ({x: i, y: j}) =
    isValid i && isValid j
        where isValid x = x >= 1 && x <= boardSize


makePosition :: PositionRec -> Either String Position
makePosition rec =
    let
        sizeString = show boardSize
    in
        if isValidPositionRec rec then
            Right $ Position rec
        else
            Left $ "Out of Bounds: Position ranges from (1,1) to ("  <> sizeString <> ","  <> sizeString <> ") inclusive"


makeValidPosition :: PositionRec -> Position
makeValidPosition rec =
    either unsafeCrashWith id $ makePosition rec        


positionRec :: Position -> PositionRec
positionRec (Position x) = 
    x


adjacentPositions :: Position -> List Position
adjacentPositions (Position ({x: i, y: j})) =
    fromFoldable 
        [ {x: i-1, y: j-1}, {x: i, y: j-1}, {x: i+1, y: j-1}
        , {x: i-1, y: j  },                 {x: i+1, y: j  }
        , {x: i-1, y: j+1}, {x: i, y: j+1}, {x: i+1, y: j+1}
        ]
            # map makePosition
            # rights


radiatingPositionRows :: Position -> List PositionRow
radiatingPositionRows pos =
    -- does NOT include starting position
    let 
        candidates = fromFoldable
            ( [rowVertUp pos] <>
              [rowVertDown pos] <>

              [rowHorizRight pos] <>
              [rowHorizLeft pos] <>

              [rowDiagUpRight pos] <>
              [rowDiagUpLeft pos] <>

              [rowDiagDownRight pos] <>
              [rowDiagDownLeft pos]       
            )
    in
        candidates
            # filter (\ (PositionRow row) -> not $ null row) 

        
rowVertUp :: Position -> PositionRow
rowVertUp (Position ({x: i, y: j})) = 
    let
        array = do  
            i' <- Array.reverse (haskellRange 1 (i-1))
            j' <- [j]
            pure (Position {x: i', y: j'})
    in
        PositionRow $ fromFoldable array


rowVertDown :: Position -> PositionRow
rowVertDown (Position ({x: i, y: j})) =
    let
        array = do  
            i' <- haskellRange (i+1) boardSize
            j' <- [j]
            pure (Position {x: i', y: j'})
    in
        PositionRow $ fromFoldable array


rowHorizRight :: Position -> PositionRow
rowHorizRight (Position ({x: i, y: j})) = 
    let
        array = do  
            i' <- [i]
            j' <- haskellRange (j+1) boardSize
            pure (Position {x: i', y: j'})
    in
        PositionRow $ fromFoldable array


rowHorizLeft :: Position -> PositionRow
rowHorizLeft (Position ({x: i, y: j})) = 
    let
        array = do  
            i' <- [i]
            j' <- Array.reverse (haskellRange 1 (j-1))  
            pure (Position {x: i', y: j'})
    in
        PositionRow $ fromFoldable array


rowDiagUpRight :: Position -> PositionRow
rowDiagUpRight (Position ({x: i, y: j})) = do  
    rowDiag Dec Inc {x: i-1, y: j+1}    


rowDiagUpLeft :: Position -> PositionRow
rowDiagUpLeft (Position ({x: i, y: j})) = do  
    rowDiag Dec Dec {x: i-1, y: j-1}      


rowDiagDownRight :: Position -> PositionRow
rowDiagDownRight (Position ({x: i, y: j})) = do  
    rowDiag Inc Inc {x: i+1, y: j+1}      


rowDiagDownLeft :: Position -> PositionRow
rowDiagDownLeft (Position ({x: i, y: j})) = do  
    rowDiag Inc Dec {x: i+1, y: j-1}    


rowDiag :: Dir -> Dir -> PositionRec -> PositionRow
rowDiag horizDir vertDir ({x: i, y: j}) =
    let
        isExceededLimits :: Int -> Int -> Boolean
        isExceededLimits = \ x y -> 
            case {h: horizDir, v: vertDir} of
                ({h: Inc, v: Inc}) -> (x > boardSize) || (y > boardSize)
                ({h: Inc, v: Dec}) -> (x > boardSize) || (y < 1)
                ({h: Dec, v: Inc}) -> (x < 1)         || (y > boardSize)
                ({h: Dec, v: Dec}) -> (x < 1)         || (y < 1)

        f :: Dir -> (Int -> Int)
        f = \ dir -> 
            case dir of
                Inc -> ( 1 + _) 
                Dec -> (-1 + _) 
            
        go result x y = 
            if isExceededLimits x y then
                result
            else
                let 
                    x' = f horizDir x
                    y' = f vertDir y
                in 
                    go (snoc result $ Position {x: x, y: y}) x' y'
    in
        PositionRow $ go Nil i j    