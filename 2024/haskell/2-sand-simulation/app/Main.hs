import Graphics.Gloss qualified as G
import Graphics.Gloss.Data.ViewPort

window :: G.Display
window = G.InWindow "Pong" (width, height) (offset, offset)

width, height, offset, fps :: Int
width = 500
height = 500
offset = 0
fps = 144

background :: G.Color
background = G.makeColorI 40 40 40 255

main :: IO ()
main = G.simulate window background fps initialState render update

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState =
  Game
    { ballLoc = (0, 0),
      ballVel = (0, -10),
      player1 = 40,
      player2 = -80
    }

update :: ViewPort -> Float -> PongGame -> PongGame
update _ = moveBall

-- | Convert a game state into a picture.
render ::
  -- | The game state to render.
  PongGame ->
  -- | A picture of this game state.
  G.Picture
render game =
  G.pictures
    [ ball,
      walls,
      mkPaddle G.rose 120 $ player1 game,
      mkPaddle G.orange (-120) $ player2 game
    ]
  where
    --  The pong ball.
    ball = uncurry G.translate (ballLoc game) $ G.color ballColor $ G.circleSolid 10
    ballColor = G.dark G.red

    --  The bottom and top walls.
    wall :: Float -> G.Picture
    wall wallDistance =
      G.translate 0 wallDistance $
        G.color wallColor $
          G.rectangleSolid 270 10

    wallColor = G.greyN 0.5
    walls = G.pictures [wall wallDistance, wall (-wallDistance)]
      where
        wallDistance = 200

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: G.Color -> Float -> Float -> G.Picture
    mkPaddle col x y =
      G.pictures
        [ G.translate x y $ G.color col $ G.rectangleSolid 26 86,
          G.translate x y $ G.color paddleColor $ G.rectangleSolid 20 80
        ]

    paddleColor = G.blue

-- | Update the ball position using its current velocity.
moveBall ::
  -- | The number of seconds since last update
  Float ->
  -- | The initial game state
  PongGame ->
  -- | A new game state with an updated ball position
  PongGame
moveBall seconds game = game {ballLoc = (x', y')}
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Data describing the state of the pong game.
data PongGame = Game
  { -- | Pong ball (x, y) location.
    ballLoc :: (Float, Float),
    -- | Pong ball (x, y) velocity.
    ballVel :: (Float, Float),
    -- | Left player paddle height. Zero is the middle of the screen.
    player1 :: Float,
    -- | Right player paddle height.
    player2 :: Float
  }
  deriving (Show)
