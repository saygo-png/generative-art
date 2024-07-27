-- Imports. {{{
-- [S]et
import Data.Array
import Data.Set qualified as S
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- }}}

-- Settings.  {{{
width, height, offset, fps :: Int
width = 500
height = width
offset = 0
fps = 144

initialState :: State
initialState =
  MkState
    { cellCount = 3
    , gap = 10
    , cellSize = widthF / cellCount initialState
    , cellMatrix = createCellWire initialState
    , keys = S.empty
    }

{- ORMOLU_DISABLE -}
-- Functions which take an alpha 0-255 value and return a transparent color. [g]ruvbox.
gDark1, gDark2, gDark3, gDark4, gLight5, gLight6, gLight7, gLight8, gRed, gOrange, gYellow, gGreen, gCyan, gBlue, gPurple, gBrown :: Int -> Color
gDark1  = makeColorI 40  40  40  -- #282828
gDark2  = makeColorI 60  56  54  -- #3c3836
gDark3  = makeColorI 80  73  69  -- #504945
gDark4  = makeColorI 102 92  84  -- #665c54
gLight5 = makeColorI 189 174 147 -- #bdae93
gLight6 = makeColorI 213 196 161 -- #d5c4a1
gLight7 = makeColorI 235 219 178 -- #ebdbb2
gLight8 = makeColorI 251 241 199 -- #fbf1c7
gRed    = makeColorI 251 73  52  -- #fb4934
gOrange = makeColorI 254 128 25  -- #fe8019
gYellow = makeColorI 250 189 47  -- #fabd2f
gGreen  = makeColorI 184 187 38  -- #b8bb26
gCyan   = makeColorI 142 192 124 -- #8ec07c
gBlue   = makeColorI 131 165 152 -- #83a598
gPurple = makeColorI 211 134 155 -- #d3869b
gBrown  = makeColorI 214 93  14  -- #d65d0e
{- ORMOLU_ENABLE -}

widthF, heightF :: Float
widthF = fromIntegral width
heightF = fromIntegral height

background :: Color
background = gDark1 255

-- }}}

-- Boilerplate. {{{
main :: IO ()
main =
  play
    window
    background
    fps
    initialState
    render
    handleInput
    update

window :: Display
window = InWindow "Sand" (width, height) (offset, offset)

-- }}}

-- Main loop. {{{

-- | Update the game state.
update :: Float -> State -> State
update _ = turnCellOn 3 2

-- | Render the game state.
render :: State -> Picture
render state =
  let cellSize' = cellSize state
      gap' = gap state
      cellCount' = cellCount state
      centerAmount = (-((widthF / 2) - cellSize' / 2 + ((gap' * cellCount') / 2)))
   in pictures
        [ color (gDark2 127) (rectangleSolid widthF heightF)
        , translate centerAmount centerAmount $ pictures (fillCells state)
        ]

-- }}}

-- Data Structures. {{{
data State = MkState
  { cellCount :: Float
  -- ^ Amount of cells
  , cellSize :: Float
  -- ^ Size of one cell
  , gap :: Float
  -- ^ Gap between the cells, makes the grid larger.
  , cellMatrix :: [[Cell]]
  -- ^ multidimensional array of points, x = column, y = row.
  , keys :: S.Set Key
  -- ^ Which keys are pressed
  }

-- | Type defining a cell on a grid, a cell can be on or off, true = on, false = off.
type Cell = (Point, Bool)

-- }}}

-- Grid setup. {{{

-- | Creates a multidimensional array of points, x = column, y = row, 1 indexed
createCellWireNew :: State -> Array (Int, Int) Cell
createCellWireNew state =
  let
    -- Generate the elements of the array
    rows = round (cellCount state)
    columns = round (cellCount state)
    cellDistance = cellSize state + gap state
    elements = [((r, c), ((fromIntegral r * cellDistance, fromIntegral c * cellDistance), False)) | r <- [0 .. rows - 1], c <- [0 .. columns - 1]]
   in
    array ((0, 0), (rows - 1, columns - 1)) elements

-- | Creates a multidimensional array of points, x = column, y = row
createCellWire :: State -> [[Cell]]
createCellWire grid = gridWireIntoCells . horizontalLineCells $ verticalLineCells cellCount' 0
 where
  cellCount' = cellCount grid
  cellSize' = cellSize grid
  gap' = gap grid
  verticalLineCells :: Float -> Float -> [Point]
  verticalLineCells 0 _ = []
  verticalLineCells n acc = (0, acc) : verticalLineCells (n - 1) (acc + cellSize' + gap')

  horizontalLineCells :: [Point] -> [[Point]]
  horizontalLineCells = map (toRow cellCount' 0)
   where
    toRow :: Float -> Float -> Point -> [Point]
    toRow 0 _ _ = []
    toRow n acc (vx, vy) = (vx + acc, vy) : toRow (n - 1) (acc + cellSize' + gap') (vx, vy)

  -- \| Converts a Point grid to a Cell grid.
  gridWireIntoCells :: [[Point]] -> [[Cell]]
  gridWireIntoCells = map (map pointToCell)
   where
    -- \| Converts a Point to a Cell.
    pointToCell :: Point -> Cell
    pointToCell (x, y) = ((x, y), False)

-- | Draw a cell on each point.
fillCells :: State -> [Picture]
fillCells grid = concatMap (map rectOnCell) (cellMatrix grid)
 where
  cellSize' = cellSize grid
  rectOnCell :: Cell -> Picture
  rectOnCell ((x, y), True) = translate x y $ color (gGreen 255) (rectangleSolid cellSize' cellSize')
  rectOnCell ((x, y), False) = translate x y $ color (gDark3 255) (rectangleSolid cellSize' cellSize')

-- }}}

-- Cell Logic. {{{

-- | Make the cell fall
fall :: Cell -> State
fall = undefined

-- | Get a cell based on row, column. 1 indexed.
getCell :: Int -> Int -> State -> Cell
getCell rowIndex colIndex state =
  let array = cellMatrix state
      rowIndex' = rowIndex - 1
      colIndex' = colIndex - 1
   in ((array !! rowIndex') !! colIndex')

filterTrue :: [[Cell]] -> [Cell]
filterTrue nestedList = filter (\((_, _), b) -> b) (concat nestedList)

-- | Update an element at a specific position, 1 indexed
turnCellOn :: Int -> Int -> State -> State
turnCellOn rowIndex' colIndex' grid = grid{cellMatrix = cellMatrix'}
 where
  list = cellMatrix grid
  rowIndex = rowIndex' - 1
  colIndex = colIndex' - 1
  -- Extract the row
  row = list !! rowIndex
  -- Update the specific element in the row
  updatedRow = take colIndex row ++ [updatedElement] ++ drop (colIndex + 1) row
   where
    updatedElement = let ((x, y), _) = row !! colIndex in ((x, y), True)
  -- Reconstruct the list with the updated row
  cellMatrix' = take rowIndex list ++ [updatedRow] ++ drop (rowIndex + 1) list


-- }}}

-- Input Handling. {{{

-- | Respond to key events.
handleInput :: Event -> State -> State

-- | Register j as a key press.
handleInput (EventKey (Char 'j') Down _ _) game =
  game{keys = S.insert (Char 'j') (keys game)}
handleInput (EventKey (Char 'j') Up _ _) game =
  game{keys = S.delete (Char 'j') (keys game)}
-- Do nothing for all other events.
handleInput _ game = game

-- }}}

-- vim:foldmethod=marker
