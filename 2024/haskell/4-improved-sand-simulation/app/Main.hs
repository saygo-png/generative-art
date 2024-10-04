-- Imports. {{{
import Data.Array
import Data.Array.Base ()
import Data.Foldable
import Data.Set qualified as S
import Data.Tuple ()
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- }}}

-- Settings.  {{{
width, height, offset, fps :: Int
fps = 30
offset = 0
width = 500
height = width

falseColor, background :: Color
background = gDark1 255
falseColor = gDark3 255

cellSize, cellCount, gap, widthF :: Float
gap = 0
cellCount = 50
cellSize = widthF / cellCount
widthF = fromIntegral width

initialState :: State
initialState =
  MkState
    { cellMatrix = cellWire,
      spawnLoc = (5, 5),
      keys = S.empty,
      colors =
        let colorsArray = [
              {- ORMOLU_DISABLE -}
                gDark1 255,
                gDark2 255,
                gDark3 255,
                gDark4 255,
                gLight5 255,
                gLight6 255,
                gLight7 255,
                gLight8 255
                -- gRed 255,
                -- gOrange 255,
                -- gYellow 255,
                -- gGreen 255,
                -- gCyan 255,
                -- gBlue 255,
                -- gPurple 255,
                -- gBrown 255
                {- ORMOLU_ENABLE -}
              ]
        in  listArray (0, length colorsArray - 1) colorsArray,
      colorIndex = 0
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
update _ state = fall . shiftColor . spawnAtLoc $ moveSpawner state

-- | Render the game state.
render :: State -> Picture
render state =
  let centerAmount = (-((widthF / 2) - cellSize / 2 + ((gap * cellCount) / 2)))
  in  translate centerAmount centerAmount . pictures $ fillCellsNew state ++ [showCursor state]

-- }}}

-- Data Structures. {{{
data State = MkState
  { -- | multidimensional array of points, x = column, y = row.
    cellMatrix :: Array (Int, Int) Cell,
    -- | Location of the spawning point of sand
    spawnLoc :: (Int, Int),
    -- | Which keys are pressed
    keys :: S.Set Key,
    -- | Colors of the cells
    colors :: Array Int Color,
    -- | Current color
    colorIndex :: Int
  }

-- | Type defining a cell on a grid, a cell can be on or off, true = on, false = off.
type Cell = (Point, Bool, Color)

-- }}}

-- Grid setup. {{{

-- | Creates a multidimensional array of points
cellWire :: Array (Int, Int) Cell
cellWire =
  let -- Generate the elements of the array
      rows = round (cellCount - 1)
      columns = round (cellCount - 1)
      cellDistance = cellSize + gap
      elements =
        [ ((rows', cols'), ((fromIntegral rows' * cellDistance, fromIntegral cols' * cellDistance), False, falseColor))
          | rows' <- [0 .. rows],
            cols' <- [0 .. columns]
        ]
  in  array ((0, 0), (rows, columns)) elements

-- | Draw a cell on each point.
fillCellsNew :: State -> [Picture]
fillCellsNew grid = toList (fmap rectOnCell (cellMatrix grid))
  where
    rectOnCell :: Cell -> Picture
    rectOnCell ((x, y), _, color') = translate x y $ color color' (rectangleSolid cellSize cellSize)

-- }}}

-- Cell Logic. {{{

-- | Make the cell fall
fall :: State -> State
fall state = state{cellMatrix = cellMatrix state // updates}
  where
    updates = concatMap fallCell [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]
    grid = cellMatrix state
    ((minX, minY), (maxX, maxY)) = bounds grid

    fallCell :: (Int, Int) -> [((Int, Int), Cell)]
    fallCell (x, y)
      -- Edge cases for y values to avoid out of bounds errors
      | y == minY && isEmpty currentCell && isSand aboveCell = [((x, y), (point, True, aboveColor)), ((x, y + 1), (abovePoint, False, falseColor))]
      | y == minY = []
      | isSand currentCell && isEmpty belowCell = [((x, y), (point, False, falseColor)), ((x, y - 1), (belowPoint, True, color'))]
      -- Edge cases for x values
      | x == minX && isSand currentCell && isEmpty belowRightCell = [((x, y), (point, False, falseColor)), ((x + 1, y - 1), (belowRightPoint, True, color'))]
      | x == maxX && isSand currentCell && isEmpty belowLeftCell = [((x, y), (point, False, falseColor)), ((x - 1, y - 1), (belowLeftPoint, True, color'))]
      | x == minX || x == maxX = []
      | isSand currentCell && isEmpty belowLeftCell = [((x, y), (point, False, falseColor)), ((x - 1, y - 1), (belowLeftPoint, True, color'))]
      | isSand currentCell && isEmpty belowRightCell = [((x, y), (point, False, falseColor)), ((x + 1, y - 1), (belowRightPoint, True, color'))]
      | otherwise = []
      where
        currentCell = grid ! (x, y)
        (point, _, color') = currentCell

        belowCell = grid ! (x, y - 1)
        (belowPoint, _, _) = belowCell

        belowLeftCell = grid ! (x - 1, y - 1)
        (belowLeftPoint, _, _) = belowLeftCell

        belowRightCell = grid ! (x + 1, y - 1)
        (belowRightPoint, _, _) = belowRightCell

        aboveCell = grid ! (x, y + 1)
        (abovePoint, _, aboveColor) = aboveCell

        -- Check if a cell contains sand
        isSand (_, on, _) = on

        -- Check if a cell is empty
        isEmpty (_, on, _) = not on

-- }}}

-- Helper Functions. {{{

shiftColor :: State -> State
shiftColor state = state{colorIndex = if colorIndex' < maxIdx then colorIndex' + 1 else 0}
  where
    colorArr = colors state
    (_, maxIdx) = bounds colorArr
    colorIndex' = colorIndex state

-- }}}

-- Input Handling. {{{

showCursor :: State -> Picture
showCursor state =
  translate x y $ color (gDark1 255) $ rectangleSolid cellSize cellSize
  where
    spawnLoc' = spawnLoc state
    arr = cellMatrix state
    ((x, y), _, _) = arr ! spawnLoc'

spawnAtLoc :: State -> State
spawnAtLoc state = state{cellMatrix = arr // [(spawnLoc', (point, True, color'))]}
  where
    arr = cellMatrix state
    spawnLoc' = spawnLoc state
    colorIndex' = colorIndex state
    color' = colors state ! colorIndex'
    (point, _, _) = arr ! spawnLoc'

moveSpawner :: State -> State
moveSpawner state
  | S.member (Char 'h') (keys state) =
      if currentX <= minX
        then -- Do nothing
          state
        else -- Update position
          state{spawnLoc = (currentX - 1, currentY)}
  | S.member (Char 'j') (keys state) =
      if currentY <= minY
        then -- Do nothing
          state
        else -- Update position
          state{spawnLoc = (currentX, currentY - 1)}
  | S.member (Char 'k') (keys state) =
      if currentY >= maxY
        then -- Do nothing
          state
        else -- Update position
          state{spawnLoc = (currentX, currentY + 1)}
  | S.member (Char 'l') (keys state) =
      if currentX >= maxX
        then -- Do nothing
          state
        else -- Update position
          state{spawnLoc = (currentX + 1, currentY)}
  | otherwise = state
  where
    ((minX, minY), (maxX, maxY)) = bounds $ cellMatrix state
    (currentX, currentY) = spawnLoc state

-- | Respond to key events.
handleInput :: Event -> State -> State

-- | Register hjkl as a key presses.
handleInput (EventKey (Char 'h') Down _ _) game =
  game{keys = S.insert (Char 'h') (keys game)}
handleInput (EventKey (Char 'h') Up _ _) game =
  game{keys = S.delete (Char 'h') (keys game)}
handleInput (EventKey (Char 'j') Down _ _) game =
  game{keys = S.insert (Char 'j') (keys game)}
handleInput (EventKey (Char 'j') Up _ _) game =
  game{keys = S.delete (Char 'j') (keys game)}
handleInput (EventKey (Char 'k') Down _ _) game =
  game{keys = S.insert (Char 'k') (keys game)}
handleInput (EventKey (Char 'k') Up _ _) game =
  game{keys = S.delete (Char 'k') (keys game)}
handleInput (EventKey (Char 'l') Down _ _) game =
  game{keys = S.insert (Char 'l') (keys game)}
handleInput (EventKey (Char 'l') Up _ _) game =
  game{keys = S.delete (Char 'l') (keys game)}
-- Do nothing for all other events.
handleInput _ game = game

-- }}}

-- vim:foldmethod=marker
