import Graphics.Gloss qualified as G
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S

-- Settings --
width, height, offset, fps :: Int
width = 500
height = 500
offset = 0
fps = 144

radius, wallDistance, paddleDistance, paddleThickness, paddleLength, wallThickness :: Float

-- | Size of the ball.
radius = 10

-- | Distance between the walls.
wallDistance = 240

-- | Distance between the paddles.
paddleDistance = 200

-- | Paddle thickness.
paddleThickness = 10

-- | Paddle thickness.
paddleLength = 100

-- | Wall thickness.
wallThickness = 40

-- Settings --

window :: G.Display
window = G.InWindow "Pong" (width, height) (offset, offset)

background :: G.Color
background = G.makeColorI 40 40 40 255

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game state.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds

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
      mkPaddle (G.makeColorI 254 128 25 255) (-paddleDistance) $ player1 game,
      mkPaddle (G.makeColorI 184 187 38 255) paddleDistance $ player2 game
    ]
  where
    --  The pong ball.
    ball = uncurry G.translate (ballLoc game) $ G.color ballColor $ G.circleSolid radius
    ballColor = G.makeColorI 255 73 52 255

    --  The bottom and top walls.
    wall :: Float -> G.Picture
    wall wallDistance' =
      G.translate 0 wallDistance' $
        G.color wallColor $
          G.rectangleSolid (fromIntegral width) wallThickness

    wallColor = G.makeColorI 131 165 152 255
    walls = G.pictures [wall wallDistance, wall (-wallDistance)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: G.Color -> Float -> Float -> G.Picture
    mkPaddle paddleColor x y =
      G.pictures [G.translate x y $ G.color paddleColor $ G.rectangleSolid paddleThickness paddleLength]

-- | Update the ball position using its current velocity.
moveBall ::
  -- | The number of seconds since last update
  Float ->
  -- | The initial game state
  PongGame ->
  -- | A new game state with an updated ball position
  PongGame
moveBall seconds game = game {ballLoc = (x', y'), ballVel = (vx', vy')}
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
    -- New veocities.
    vx' = vx
    vy' = vy

type Position = (Float, Float)

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game {ballVel = (vx', vy)}
  where
    -- The old velocities.
    (vx, vy) = ballVel game

    vx' =
      if paddleCollision $ ballLoc game
        then -- Update the velocity.
          -vx
        else -- Do nothing. Return the old velocity.
          vx

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game {ballVel = (vx, vy')}
  where
    -- The old velocities.
    (vx, vy) = ballVel game

    vy' =
      if wallCollision $ ballLoc game
        then -- Update the velocity.
          -vy
        else -- Do nothing. Return the old velocity.
          vy

-- | Given position and radius of the ball and the distance between paddles, return whether a collision occurred.
paddleCollision :: Position -> Bool
paddleCollision (x, _) = leftCollision || rightCollision
  where
    leftCollision = x - radius <= -(paddleDistance - paddleThickness / 2)
    rightCollision = x + radius >= paddleDistance - paddleThickness / 2

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    topCollision = y - radius <= -(wallDistance - wallThickness / 2)
    bottomCollision = y + radius >= wallDistance - wallThickness / 2

-- moveDown :: PongGame -> PongGame
-- moveDown
--

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'j') Down _ _) game =
  game { keys = S.insert (Char 'j') (keys game)}
handleKeys (EventKey (Char 'j') Up _ _) game =
  game { keys = S.delete (Char 'j') (keys game)}
  -- game {player1 = y'}
  -- where
  --   y = player1 game
  --
  --   y' =
  --     if wallCollision (1.0, player1 game)
  --       then -- Update the velocity.
  --         y
  --       else -- Do nothing. Return the old velocity.
  --         y - 5

handleKeys (EventKey (Char 'k') Down _ _) game =
  game { keys = S.insert (Char 'k') (keys game)}
handleKeys (EventKey (Char 'k') Up _ _) game =
  game { keys = S.delete (Char 'k') (keys game)}
-- Do nothing for all other events.
handleKeys _ game = game

-- | Data describing the state of the pong game.
data PongGame = Game
  { -- | Pong ball (x, y) location.
    ballLoc :: (Float, Float),
    -- | Pong ball (x, y) velocity.
    ballVel :: (Float, Float),
    -- | Left player paddle height.
    player1 :: Float,
    -- | Right player paddle height.
    player2 :: Float,
    -- | Which keys are pressed.
    keys :: S.Set Key
  }
  deriving (Show)

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState =
  Game
    { ballLoc = (0, 20),
      ballVel = (-300, 300),
      player1 = 40,
      player2 = -80,
      keys = S.empty
    }
