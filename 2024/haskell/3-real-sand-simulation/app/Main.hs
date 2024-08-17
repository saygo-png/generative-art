-- Imports. {{{
import Data.Array
import Data.Array.Base qualified as A
import Data.Foldable
import Data.Set qualified as S
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- }}}

-- Settings.  {{{
width, height, offset, fps :: Int
width = 500
height = width
offset = 0
fps = 3

initialState :: State
initialState =
  MkState
    { cellCount = 50,
      gap = 0,
      cellSize = widthF / cellCount initialState,
      cellMatrix = createCellWireNew initialState,
      spillDir = "left",
      spawnLoc = (5, 5),
      keys = S.empty,
      colors =
        listArray
          (0, 7)
          [
            -- (gDark1 255),
            -- (gDark2 255),
            -- (gDark3 255), (gDark4 255),
            -- (gLight5 255),
            -- (gLight6 255),
            -- (gLight7 255),
            -- (gLight8 255)
            (gRed 255),
            (gOrange 255),
            (gYellow 255),
            (gGreen 255),
            (gCyan 255),
            (gBlue 255),
            (gPurple 255),
            (gBrown 255)
          ],
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
update _ state = spill . fall . spawnAtLoc $ moveSpawner state

-- | Render the game state.
render :: State -> Picture
render state =
  let cellSize' = cellSize state
      gap' = gap state
      cellCount' = cellCount state
      centerAmount = (-((widthF / 2) - cellSize' / 2 + ((gap' * cellCount') / 2)))
  in  pictures
        [ color (gDark2 127) (rectangleSolid widthF heightF),
          translate centerAmount centerAmount $ pictures (fillCellsNew state)
        ]

-- }}}

-- Data Structures. {{{
data State = MkState
  { -- | Amount of cells
    cellCount :: Float,
    -- | Size of one cell
    cellSize :: Float,
    -- | Gap between the cells, makes the grid larger.
    gap :: Float,
    -- | multidimensional array of points, x = column, y = row.
    cellMatrix :: Array (Int, Int) Cell,
    -- | Direction of spilling
    spillDir :: [Char],
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
createCellWireNew :: State -> Array (Int, Int) Cell
createCellWireNew state =
  let -- Generate the elements of the array
      rows = round (cellCount state - 1)
      columns = round (cellCount state - 1)
      cellDistance = cellSize state + gap state
      elements =
        [ ((r, c), ((fromIntegral r * cellDistance, fromIntegral c * cellDistance), False, rose))
          | r <- [0 .. rows],
            c <- [0 .. columns]
        ]
  in  array ((0, 0), (rows, columns)) elements

-- | Draw a cell on each point.
fillCellsNew :: State -> [Picture]
fillCellsNew grid = toList (fmap rectOnCell (cellMatrix grid))
  where
    cellSize' = cellSize grid
    rectOnCell :: Cell -> Picture
    rectOnCell ((x, y), True, color') = translate x y $ color color' (rectangleSolid cellSize' cellSize')
    rectOnCell ((x, y), False, _) = translate x y $ color (gDark3 255) (rectangleSolid cellSize' cellSize')

-- }}}

-- Cell Logic. {{{

-- | Spill cell left then right
spill :: State -> State
spill state
  | spillDir state == "left" = (spillLeft state){spillDir = "right"}
  | spillDir state == "right" = (spillRight state){spillDir = "left"}
  | otherwise = error "String must be of value left or right"

-- | Spill cell left.
spillLeft :: State -> State
spillLeft state = state{cellMatrix = A.genArray (bounds arr) spillCell}
  where
    arr = cellMatrix state
    inBounds' = inBounds arr
    ((minX, minY), (maxX, maxY)) = bounds arr
    spillCell :: (Int, Int) -> Cell
    spillCell (x, y)
      | inBounds' x && y < maxY && not value && rightBlock = (point, True, colorRight)
      | y > minY && inRange (minX + 2, maxX - 2) x && value && valueBelow && not valueLeftBelow && not valueLeftLeftBelow = (point, False, color')
      | x == minX && y > minY && y < maxY && not value && rightBlock = (point, True, colorRight)
      | x == minX + 1 && y > minY && y < maxY && value && valueBelow && not valueLeftBelow = (point, False, color')
      | otherwise = (point, value, color')
      where
        rightBlock = valueRight && valueRightAbove

        (point, value, color') = arr ! (x, y)

        (_, valueBelow, colorBelow) = arr ! (x, y - 1)

        (_, valueRight, colorRight) = arr ! (x + 1, y)
        (_, valueRightAbove, colorRightAbove) = arr ! (x + 1, y + 1)

        (_, valueLeftBelow, colorLeftBelow) = arr ! (x - 1, y - 1)
        (_, valueLeftLeftBelow, colorLeftLeftBelow) = arr ! (x - 2, y - 1)

-- | Spill cell right.
spillRight :: State -> State
spillRight state = state{cellMatrix = A.genArray (bounds arr) spillCell}
  where
    arr = cellMatrix state
    inBounds' = inBounds arr
    ((minX, minY), (maxX, maxY)) = bounds arr
    spillCell :: (Int, Int) -> Cell
    spillCell (x, y)
      | inBounds' x && y < maxY && not value && leftBlock = (point, True, colorLeft)
      | y > minY && inRange (minX + 2, maxX - 2) x && value && valueBelow && not valueRightBelow && not valueRightRightBelow = (point, False, color')
      | x == maxX && y > minY && y < maxY && not value && leftBlock = (point, True, colorLeft)
      | x == maxX - 1 && y > minY && y < maxY && value && valueBelow && not valueRightBelow = (point, False, color')
      | otherwise = (point, value, color')
      where
        leftBlock = valueLeft && valueLeftAbove

        (point, value, color') = arr ! (x, y)

        (_, valueBelow, colorBelow) = arr ! (x, y - 1)

        (_, valueRightBelow, colorRightBelow) = arr ! (x + 1, y - 1)
        (_, valueRightRightBelow, colorRightRightBelow) = arr ! (x + 2, y - 1)

        (_, valueLeft, colorLeft) = arr ! (x - 1, y)
        (_, valueLeftAbove, colorLeftAbove) = arr ! (x - 1, y + 1)

-- | Make the cell fall
fall :: State -> State
fall state = state{cellMatrix = A.genArray (bounds arr) fallCell}
  where
    arr = cellMatrix state
    inBounds' = inBounds arr
    ((minX, minY), (maxX, maxY)) = bounds arr
    fallCell :: (Int, Int) -> Cell
    fallCell (x, y)
      | y == maxY && value && valueBelow = (point, True, color')
      | y == minY && value = (point, True, color')
      | y == minY && not value && valueAbove = (point, True, colorAbove)
      | inBounds' y && value && valueBelow = (point, True, color')
      | inBounds' y && not value && valueAbove = (point, True, colorAbove)
      | otherwise = (point, False, color')
      where
        (point, value, color') = arr ! (x, y)
        (_, valueBelow, colorBelow) = arr ! (x, y - 1)
        (_, valueAbove, colorAbove) = arr ! (x, y + 1)

-- }}}

-- Helper Functions. {{{
inBounds :: Array (Int, Int) Cell -> Int -> Bool
inBounds arr number =
  let ((minX, minY), (maxX, maxY)) = bounds arr
  in  inRange (minY + 1, maxY - 1) number && inRange (minX + 1, maxX - 1) number

-- | Update a cell at a specific position
turnCellOn :: (Ix i) => i -> Array i Cell -> Array i Cell
turnCellOn idx arr = arr // [(idx, (point, True, color'))]
  where
    (point, _, color') = arr ! idx

-- | Update a cells color at a specific position
colorCell :: (Ix i) => Color -> i -> Array i Cell -> Array i Cell
colorCell color' idx arr = arr // [(idx, (point, True, color'))]
  where
    (point, _, _) = arr ! idx

-- }}}

-- Input Handling. {{{
spawnAtLoc :: State -> State
spawnAtLoc state =
  state
    { cellMatrix = colorCell color' spawnLoc' $ turnCellOn spawnLoc' arr,
      colorIndex = if colorIndex' < maxIdx
                   then colorIndex' + 1
                   else 0
    }
  where
    arr = cellMatrix state
    colorArr = colors state
    spawnLoc' = spawnLoc state
    colorIndex' = colorIndex state
    color' = colors state ! colorIndex'
    (minIdx, maxIdx) = bounds colorArr

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
