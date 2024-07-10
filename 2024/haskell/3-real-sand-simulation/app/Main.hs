import Data.Set qualified as S -- [S]et
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import RIO hiding (Display, Down)

--------------
-- Settings --
--------------

width, height, offset, fps :: Int
width = 500
height = width
offset = 0
fps = 144

widthF, heightF :: Float
widthF = fromIntegral width
heightF = fromIntegral height

background :: Color
background = makeColorI 40 40 40 255

-----------------
-- Boilerplate --
-----------------

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

--

-- | Update the game state.
update :: Float -> SandSimulation -> SandSimulation
update _ game = game

render :: SandSimulation -> Picture
render _ = let
    centerAmount = (-((widthF / 2)))
  in
  pictures
    [ color (greyN 0.3) (rectangleSolid widthF heightF),
      translate centerAmount centerAmount $ pictures (fillGrid (makeColorI 250 189 47 255) (createGridWire (getCellCount initialGrid)))
    ]

data Grid = MkGrid
  { -- | Amount of cells
    getCellCount :: Float,
    getCellSize :: Float,
    getGap :: Float
  }

initialGrid :: Grid
initialGrid =
  MkGrid
    { getCellCount = 10,
      getCellSize = widthF / getCellCount initialGrid,
      getGap = 20
    }

data SandSimulation = State
  { -- | Total sand particle amount.
    sandAmount :: Int,
    -- | Which keys are pressed
    keys :: S.Set Key
  }
  deriving (Show)

initialState :: SandSimulation
initialState =
  State
    { sandAmount = 0,
      keys = S.empty
    }

createGridWire :: Float -> [[Point]]
createGridWire cellCount = horizontalLineCells $ verticalLineCells cellCount 0
  where
    cellSize = fromIntegral width / cellCount
    gap = getGap initialGrid
    verticalLineCells :: Float -> Float -> [Point]
    verticalLineCells 0 _ = []
    verticalLineCells n acc = (0, acc) : verticalLineCells (n - 1) (acc + cellSize + gap)

    horizontalLineCells :: [Point] -> [[Point]]
    horizontalLineCells = map (toRow cellCount 0)
      where
        toRow :: Float -> Float -> Point -> [Point]
        toRow 0 _ _ = []
        toRow n acc (vx, vy) = (vx + acc, vy) : toRow (n - 1) (acc + cellSize + gap) (vx, vy)

fillGrid :: Color -> [[Point]] -> [Picture]
fillGrid color' = concatMap (map rectOnPoint)
  where
    cellCount = getCellCount initialGrid
    cellSize = fromIntegral width / cellCount
    rectOnPoint :: Point -> Picture
    rectOnPoint (x, y) = translate x y $ color color' (rectangleSolid cellSize cellSize)

-- | Respond to key events.
handleInput :: Event -> SandSimulation -> SandSimulation

-- | Register j as a key press.
handleInput (EventKey (Char 'j') Down _ _) game =
  game {keys = S.insert (Char 'j') (keys game)}
handleInput (EventKey (Char 'j') Up _ _) game =
  game {keys = S.delete (Char 'j') (keys game)}
-- Do nothing for all other events.
handleInput _ game = game
