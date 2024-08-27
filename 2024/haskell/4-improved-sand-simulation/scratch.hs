import Data.Array
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Tuple (swap)

-- Define Point and Color
type Point = (Int, Int)
type Color = (Int, Int, Int)  -- RGB color, assuming (0-255, 0-255, 0-255)

-- Type defining a cell on a grid, a cell can be on or off, true = on, false = off.
type Cell = (Point, Bool, Color)

-- A 2D grid of cells
type Grid = Array Point Cell

-- Constants for grid size
width :: Int
width = 20

height :: Int
height = 10

-- Define a basic sand color (e.g., yellowish)
sandColor :: Color
sandColor = (194, 178, 128)

-- Initialize the grid with all cells "off" and empty color
emptyGrid :: Grid
emptyGrid = array ((0, 0), (width - 1, height - 1)) 
              [((x, y), ((x, y), False, (0, 0, 0))) | x <- [0..width-1], y <- [0..height-1]]

-- Function to add sand (turn cell "on") at a specific position with a color
addSand :: Grid -> Point -> Grid
addSand grid pos = grid // [(pos, (pos, True, sandColor))]

-- Function to simulate one step of the sand falling
step :: Grid -> Grid
step grid = grid // updates
  where
    -- List of updates to perform on the grid
    updates = concatMap updateCell [(x, y) | x <- [0..width-1], y <- [0..height-2]]
    
    -- Update a single cell
    updateCell :: Point -> [(Point, Cell)]
    updateCell (x, y)
      | isSand (grid ! (x, y)) && isEmpty (grid ! (x, y + 1)) = [(swap (x, y), emptyCell), (swap (x, y + 1), sandCell (x, y + 1))]
      | isSand (grid ! (x, y)) && x > 0 && isEmpty (grid ! (x - 1, y + 1)) = [(swap (x, y), emptyCell), (swap (x - 1, y + 1), sandCell (x - 1, y + 1))]
      | isSand (grid ! (x, y)) && x < width - 1 && isEmpty (grid ! (x + 1, y + 1)) = [(swap (x, y), emptyCell), (swap (x + 1, y + 1), sandCell (x + 1, y + 1))]
      | otherwise = []

    -- Create a new sand cell
    sandCell p = (p, True, sandColor)

    -- Create an empty cell
    emptyCell = ((0, 0), False, (0, 0, 0))

    -- Check if a cell contains sand
    isSand (_, on, _) = on

    -- Check if a cell is empty
    isEmpty (_, on, _) = not on

-- Print the grid to the console
printGrid :: Grid -> IO ()
printGrid grid = do
  putStrLn $ unlines [ [cellToChar (grid ! (x, y)) | x <- [0..width-1]] | y <- [0..height-1] ]
  where
    cellToChar (_, True, _) = '#'
    cellToChar (_, False, _) = '.'

-- Main simulation loop
simulate :: Grid -> IO ()
simulate grid = do
  printGrid grid
  let grid' = step grid
  _ <- getLine  -- Wait for user input to proceed to next step
  simulate grid'

-- Function to randomly add sand to the grid
randomSand :: Grid -> IO Grid
randomSand grid = do
  n <- randomRIO (1, width - 2)
  return $ addSand grid (n, 0)

main :: IO ()
main = do
  let grid = emptyGrid
  gridWithSand <- randomSand grid
  simulate gridWithSand
