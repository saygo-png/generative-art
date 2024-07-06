import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--------------
-- Settings --
--------------

width, height, offset, fps :: Int
width = 50
height = 50
offset = 0
fps = 144

background :: Color
background = makeColorI 40 40 40 255

-----------------
-- Boilerplate --
-----------------

main :: IO ()
main = play window background fps initialState

window :: Display
window = InWindow "Sand" (width, height) (offset, offset)

--
