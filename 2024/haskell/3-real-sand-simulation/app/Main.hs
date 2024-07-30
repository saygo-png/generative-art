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
    { cellCount = 30
    , gap = 2
    , cellSize = widthF / cellCount initialState
    , cellMatrix = turnCellOn (14, 24) $ createCellWireNew initialState
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
update _ grid = fall grid

-- | Render the game state.
render :: State -> Picture
render state =
  let cellSize' = cellSize state
      gap' = gap state
      cellCount' = cellCount state
      centerAmount = (-((widthF / 2) - cellSize' / 2 + ((gap' * cellCount') / 2)))
   in pictures
        [ color (gDark2 127) (rectangleSolid widthF heightF)
        , translate centerAmount centerAmount $ pictures (fillCellsNew state)
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
  , cellMatrix :: Array (Int, Int) Cell
  -- ^ multidimensional array of points, x = column, y = row.
  , keys :: S.Set Key
  -- ^ Which keys are pressed
  }

-- | Type defining a cell on a grid, a cell can be on or off, true = on, false = off.
type Cell = (Point, Bool)

-- }}}

-- Grid setup. {{{

-- | Creates a multidimensional array of points
createCellWireNew :: State -> Array (Int, Int) Cell
createCellWireNew state =
  let
    -- Generate the elements of the array
    rows = round (cellCount state - 1)
    columns = round (cellCount state - 1)
    cellDistance = cellSize state + gap state
    elements =
      [ ((r, c), ((fromIntegral r * cellDistance, fromIntegral c * cellDistance), False))
      | r <- [0 .. rows]
      , c <- [0 .. columns]
      ]
   in
    array ((0, 0), (rows, columns)) elements

-- | Draw a cell on each point.
fillCellsNew :: State -> [Picture]
fillCellsNew grid = toList (fmap rectOnCell (cellMatrix grid))
 where
  cellSize' = cellSize grid
  rectOnCell :: Cell -> Picture
  rectOnCell ((x, y), True) = translate x y $ color (gGreen 255) (rectangleSolid cellSize' cellSize')
  rectOnCell ((x, y), False) = translate x y $ color (gDark3 255) (rectangleSolid cellSize' cellSize')

-- }}}

-- Cell Logic. {{{

-- | Make the cell fall
fall :: State -> State
fall state = state{cellMatrix = fallArray $ cellMatrix state}
 where
  fallArray :: Array (Int, Int) Cell -> Array (Int, Int) Cell
  fallArray arr = A.genArray (bounds arr) processCell
   where
    ((_, _), (_, maxY)) = bounds arr
    inBounds :: Int -> Bool
    inBounds = inRange (0 + 1, maxY - 1)
    processCell :: (Int, Int) -> Cell
    processCell (x, y)
      | inBounds y && value == False && valueAbove == True = (point, True)
      | y == 0 && value == True = (point, True)
      | y == 0 && value == False && valueAbove == True = (point, True)
      | otherwise = (point, False)
     where
      (point, value) = arr ! (x, y)
      (_, valueBelow) = arr ! (x, y - 1)
      (_, valueAbove) = arr ! (x, y + 1)

-- | Update an element at a specific position
turnCellOn :: (Ix i) => i -> Array i Cell -> Array i Cell
turnCellOn idx arr = arr // [(idx, (fst (arr ! idx), True))]

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
