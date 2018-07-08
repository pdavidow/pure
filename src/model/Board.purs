module Board
    ( Board -- hiding constructor
    , EmptySquare(..)
    , FilledSquare -- hiding constructor
    , Move(..)
    , FilledRow(..)
    , Tagged_Square(..)
    , initialBoard
    , toPosition
    , validMoves
    , boardAt
    , boardArrayAt
    , isFilledSquare
    , boardFromConfig
    , applyBoardMove
    , filledPositions
    , squaresColoredCounts_BlackWhite
    , moveColor
    , filledSquares
    , toFilledSquare
    , filledSquareColor
    , isSquareColored
    , isEmptyAt
    , boardSquaresColored
    , cornerCounts_BlackWhite
    , filledSquaresAdjacentToEmptyCorners
    , movePosition
    , movePositionChoices
    , boardElems
    , emptySquares
    , diskFrom
    , outflankPositions
    , outflankPositions_Traversing
    --, flipAt -- Should NOT be exposed (but ok to temp expose for sake of commented-out test)
    )
    where
      
import Prelude
import BoardSize (boardSize)
import Data.Array as Array
import Data.List (List(..), elem, nub, null, any, concatMap, filter, foldl, fromFoldable, head, tail, takeWhile, length, mapMaybe, range, zipWith)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Disk (Disk, Color(..), diskColor, flipDisk, makeDisk, toggleColor)
import Lib (haskellRange, mapTakeWhile) 
import Partial.Unsafe (unsafePartial)
import Position (Position, PositionRec, PositionRow(..), adjacentPositions, makeValidPosition, positionRec, radiatingPositionRows)
import BlackWhite (BlackWhite(..), makeBlackWhite)


data EmptySquare = EmptySquare 
    { position :: Position
    , radiatingPositionRows :: List PositionRow
    }

data FilledSquare = FilledSquare 
    { disk :: Disk
    , emptySquare :: EmptySquare
    }

data Tagged_Square 
    = Tagged_EmptySquare EmptySquare
    | Tagged_FilledSquare FilledSquare 

newtype Board = Board (Array (Array Tagged_Square)) -- one-based

newtype FilledRow = FilledRow (List FilledSquare)   

data Move = Move 
    { color :: Color
    , emptySquare :: EmptySquare
    , outflanks :: List FilledRow
    }
 

derive instance eqFilledSquare :: Eq FilledSquare
derive instance eqTagged_Square :: Eq Tagged_Square
derive instance eqBoard :: Eq Board
derive instance eqFilledRow :: Eq FilledRow
derive instance eqMove :: Eq Move


instance eqEmptySquare :: Eq EmptySquare where
    eq (EmptySquare rec1) (EmptySquare rec2) = 
        rec1.position == rec2.position 


instance showEmptySquare :: Show EmptySquare where
    show (EmptySquare rec) = 
        "EmptySquare> " <> show rec.position        


instance showFilledSquare :: Show FilledSquare where
    show x@(FilledSquare rec) = "FilledSquare> " <> show ((toPosition <<< Tagged_FilledSquare) x) <> " " <> show rec.disk  


instance showTagged_Square :: Show Tagged_Square where
     show taggedSquare =
        case taggedSquare of
            Tagged_EmptySquare x -> "Tagged_EmptySquare> " <> show x
            Tagged_FilledSquare x -> "Tagged_FilledSquare> " <> show x


makeEmptySquare :: PositionRec -> EmptySquare
makeEmptySquare ({x: i, y: j}) =
    EmptySquare 
        { position: pos
        , radiatingPositionRows: radiatingPositionRows pos
        }
        where pos = makeValidPosition {x: i, y: j}


makeFilledSquare :: Disk -> EmptySquare -> FilledSquare
makeFilledSquare disk emptySquare =
    FilledSquare 
        { disk: disk
        , emptySquare: emptySquare
        }


makeBoard :: Board
makeBoard = 
    Board $ map makeBoardRow $ haskellRange 1 boardSize   


makeBoardRow :: Int -> Array Tagged_Square
makeBoardRow rowNum = do  
    i <- [rowNum]
    j <- haskellRange 1 boardSize
    pure $ Tagged_EmptySquare $ makeEmptySquare {x: i, y: j}


boardFromConfig :: Array (Tuple Color Position) -> Board
boardFromConfig config =
    fromFoldable config
        # Array.foldl (\ acc ((Tuple color pos)) -> place (makeDisk color) (boardAt acc pos) acc) makeBoard


initialBoard :: Board
initialBoard = 
    boardFromConfig 
        [ Tuple White $ makeValidPosition {x: 4,  y: 4}
        , Tuple White $ makeValidPosition {x: 5,  y: 5}
        , Tuple Black $ makeValidPosition {x: 4,  y: 5}
        , Tuple Black $ makeValidPosition {x: 5,  y: 4}
        ]


place :: Disk -> Tagged_Square -> Board -> Board
place disk taggedSquare board = 
    case taggedSquare of
        Tagged_EmptySquare emptySquare -> fillAt emptySquare disk board
        Tagged_FilledSquare _ -> board        

       
fillAt :: EmptySquare -> Disk -> Board -> Board
fillAt emptySquare@(EmptySquare rec) disk board =
    updateBoardAt rec.position val board
        where val = Tagged_FilledSquare $ makeFilledSquare disk emptySquare


flipAt :: Tagged_Square -> Board -> Board
flipAt taggedSquare board =
    case taggedSquare of
        Tagged_EmptySquare _ -> board
        Tagged_FilledSquare (FilledSquare rec) -> fillAt rec.emptySquare (flipDisk rec.disk) board


updateBoardAt :: Position -> Tagged_Square -> Board -> Board
updateBoardAt pos val (Board array2D) =     
    let
        ({x: i, y: j}) = positionRec pos

        oldRow = unsafePartial $ fromJust $ Array.index array2D $ i - 1
        newRow = unsafePartial $ fromJust $ Array.updateAt (j - 1) val oldRow
    in
        Board $ unsafePartial $ fromJust $ Array.updateAt (i - 1) newRow array2D


boardAt :: Board -> Position -> Tagged_Square
boardAt (Board array2D) pos =  
    boardArrayAt array2D $ positionRec pos  


boardArrayAt :: forall a. Array (Array a) -> PositionRec -> a
boardArrayAt array2D ({x: i, y: j}) =   
    let
        row = unsafePartial $ fromJust $ Array.index array2D $ i - 1
    in
        unsafePartial $ fromJust $ Array.index row $ j - 1 


boardElems :: Board -> Array Tagged_Square
boardElems (Board array2D) =
    Array.concat array2D -- todo return List instead ?


flipCount :: Move -> Int
flipCount move =
    length $ outflankSquares move        


outflankPositions :: Move -> List Position  
outflankPositions move =
    outflankSquares move
        # map (toPosition <<< Tagged_FilledSquare)


outflankPositions_Traversing :: Position -> Move -> List Position  
outflankPositions_Traversing position move =
    outflankSquares_Traversing position move
        # map (toPosition <<< Tagged_FilledSquare)


outflankSquares :: Move -> List FilledSquare
outflankSquares (Move rec) =
    filledRowsToSquares rec.outflanks


outflankSquares_Traversing :: Position -> Move -> List FilledSquare
outflankSquares_Traversing position (Move rec) =
    rec.outflanks
        # filter (\ (FilledRow filledSquares') -> elem position $ map (toPosition <<< Tagged_FilledSquare) filledSquares')
        # filledRowsToSquares


filledRowsToSquares :: List FilledRow -> List FilledSquare
filledRowsToSquares xs =
    xs
        # (\ filledRows -> concatMap (\ (FilledRow ys) -> ys) filledRows)    
        # nub


toPosition :: Tagged_Square -> Position
toPosition taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare (EmptySquare rec) -> rec.position
        Tagged_FilledSquare (FilledSquare rec) -> (toPosition <<< Tagged_EmptySquare) rec.emptySquare       


toEmptySquare :: Tagged_Square -> Maybe EmptySquare
toEmptySquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare x  -> Just x
        Tagged_FilledSquare _ -> Nothing       


toFilledSquare :: Tagged_Square -> Maybe FilledSquare
toFilledSquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare _  -> Nothing
        Tagged_FilledSquare x -> Just x    
 
        
emptySquares :: Board -> List EmptySquare
emptySquares board =    
    board
        # boardElems
        # fromFoldable
        # mapMaybe toEmptySquare        
 
        
filledSquares :: Board -> List FilledSquare
filledSquares board =  
    board
        # boardElems
        # fromFoldable
        # mapMaybe toFilledSquare  


corners :: Board -> List Tagged_Square
corners board =
    [ {x: 1, y: 1}
    , {x: 1, y: boardSize}
    , {x: boardSize, y: boardSize}
    , {x: boardSize, y: 1}
    ]
        # map (\rec -> boardAt board $ makeValidPosition rec)    
        # fromFoldable 


emptyCorners :: Board -> List EmptySquare
emptyCorners board =
    mapMaybe toEmptySquare $ corners board


filledCorners :: Board -> List FilledSquare
filledCorners board =
    mapMaybe toFilledSquare $ corners board      


diskFrom :: FilledSquare -> Disk
diskFrom (FilledSquare rec) =  
    rec.disk


filledSquareColor :: FilledSquare -> Color
filledSquareColor filledSquare =
    diskColor $ diskFrom filledSquare


isSquareColored :: Color -> FilledSquare -> Boolean
isSquareColored color filledSquare =
    color == filledSquareColor filledSquare  


isEmptySquare :: Tagged_Square -> Boolean
isEmptySquare taggedSquare =
    case taggedSquare of 
        Tagged_EmptySquare _ -> true
        Tagged_FilledSquare _ -> false


isFilledSquare :: Tagged_Square -> Boolean
isFilledSquare taggedSquare =
    not $ isEmptySquare taggedSquare      


isEmptyAt :: Position -> Board -> Boolean
isEmptyAt pos board =
    isEmptySquare $ boardAt board pos


isFilledAt :: Position -> Board -> Boolean
isFilledAt pos board =
    isFilledSquare $ boardAt board pos    


contiguousFilledRow :: PositionRow -> Board -> FilledRow
contiguousFilledRow (PositionRow ps) board =
    ps
        # mapTakeWhile (\ p -> boardAt board p) isFilledSquare
        # mapMaybe toFilledSquare
        # FilledRow    


outflanks :: Color -> EmptySquare -> Board -> List FilledRow
outflanks color (EmptySquare rec) board =
    rec.radiatingPositionRows
        # map (\ posRow -> contiguousFilledRow posRow board)
        # filter (\ filledRow -> isFilledRowHead_Colored toggledColor filledRow && isFilledRowTail_ContainColor color filledRow)
        # map (\ (FilledRow xs) -> FilledRow $ takeWhile (\ x -> isSquareColored toggledColor x) xs)
            where toggledColor = toggleColor color        


isFilledRowHead_Colored :: Color -> FilledRow -> Boolean
isFilledRowHead_Colored color (FilledRow row) =
    case head row of
        Just square -> isSquareColored color square
        Nothing -> false


isFilledRowTail_ContainColor :: Color -> FilledRow -> Boolean
isFilledRowTail_ContainColor color (FilledRow row) =
    case tail row of
        Just squares -> any (isSquareColored color) squares
        Nothing -> false            


adjacentEmptySquares :: Tagged_Square -> Board -> List EmptySquare
adjacentEmptySquares taggedSquare board =
    toPosition taggedSquare
        # adjacentPositions
        # mapMaybe (\ pos -> toEmptySquare $ boardAt board pos)        


boardSquaresColored :: Color -> Board -> List FilledSquare
boardSquaresColored color board =
    squaresColored color $ filledSquares board


squaresColored :: Color -> List FilledSquare -> List FilledSquare
squaresColored color xs =
    filter (\ x -> isSquareColored color x) xs        


colorCount :: Color -> Board -> Int
colorCount color board =
    filledSquares board
        # squaresColored color
        # length    


squaresColoredCounts_BlackWhite :: Board -> BlackWhite Int
squaresColoredCounts_BlackWhite board =
    makeBlackWhite (colorCount Black board) (colorCount White board)         
            

moveColor :: Move -> Color
moveColor (Move rec) =
    rec.color    


movePosition :: Move -> Position
movePosition (Move rec) =
    toPosition <<< Tagged_EmptySquare $ rec.emptySquare


filledPositions :: Color -> Board -> List Position
filledPositions color board = 
    boardSquaresColored color board
        # map (\ x -> toPosition $ Tagged_FilledSquare x)    


validMove :: Color -> EmptySquare -> Board -> Maybe Move
validMove color emptySquare board = 
    let
        candidates = outflanks color emptySquare board
    in
        if null candidates then
            Nothing
        else
            Just $ Move 
                { color: color
                , emptySquare: emptySquare
                , outflanks: candidates  
                }


validMoves :: Color -> Board -> List Move
validMoves color board =
      boardSquaresColored (toggleColor color) board
        # concatMap (\ filledSquare -> adjacentEmptySquares (Tagged_FilledSquare filledSquare) board)
        # nub 
        # mapMaybe (\ emptySquare -> validMove color emptySquare board)    


applyBoardMove :: Move -> Board -> Board
applyBoardMove (Move rec) board =
    let
        disk = makeDisk rec.color
        taggedSquare = Tagged_EmptySquare rec.emptySquare

        flipOutflanks :: Board -> Board
        flipOutflanks board' =
            rec.outflanks
                # concatMap (\ (FilledRow ys) -> ys)
                # foldl (\ acc y -> flipAt (Tagged_FilledSquare y) acc) board'
    in
        board
            # place disk taggedSquare
            # flipOutflanks        
    

corners_BlackWhite :: Board -> BlackWhite (List FilledSquare)
corners_BlackWhite board =
    let
        xs = filledCorners board
        f = \ color -> filter (isSquareColored color) xs
    in
        makeBlackWhite (f Black) (f White)


cornerCounts_BlackWhite :: Board -> BlackWhite Int
cornerCounts_BlackWhite board =
    makeBlackWhite (length b) (length w)
        where (BlackWhite {black: b, white: w}) = corners_BlackWhite board         


filledSquaresAdjacentToEmptyCorners :: Board -> List FilledSquare
filledSquaresAdjacentToEmptyCorners board =
    emptyCorners board
        # concatMap (adjacentPositions <<< toPosition <<< Tagged_EmptySquare)
        # mapMaybe (toFilledSquare <<< boardAt board)        


movePositionChoices :: List Move -> List {index :: Int, position ::Position}
movePositionChoices xs =
    -- 1 based
    zipWith (\ i p -> {index: i, position: p}) (range (1 :: Int) $ length xs) $ map movePosition xs         
