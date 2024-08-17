import Data.Set qualified as S
import Graphics.Gloss qualified as G
import Graphics.Gloss.Interface.Pure.Game

--------------
-- Settings --
--------------

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
paddleDistance = 245

-- | Paddle thickness.
paddleThickness = 10

-- | Paddle thickness.
paddleLength = 100

-- | Wall thickness.
wallThickness = 40

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState =
  Game
    { ballLoc = (0, 20),
      ballVel = (-100, 300),
      player1 = 40,
      player2 = 0,
      -- Dont change.
      keys = S.empty
    }

--------------

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

window :: G.Display
window = G.InWindow "Pong" (width, height) (offset, offset)

background :: G.Color
background = G.makeColorI 40 40 40 255

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game state.
update :: Float -> PongGame -> PongGame
update seconds game = ballOutOfBounds $ movePaddle seconds $ paddleBounce $ wallBounce $ moveBall seconds game

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
      mkPaddle (G.makeColorI 254 128 25 255) paddleThickness paddleLength (-paddleDistance) $ player1 game,
      mkPaddle (G.makeColorI 131 165 152 255) wallThickness wallPaddleLength paddleDistance $ player2 game
    ]
  where
    wallPaddleLength = fromIntegral height + 20
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
    mkPaddle :: G.Color -> Float -> Float -> Float -> Float -> G.Picture
    mkPaddle paddleColor' paddleThickness' paddleLength' x y =
      G.pictures [G.translate x y $ G.color paddleColor' $ G.rectangleSolid paddleThickness' paddleLength']

-- | Update the paddle position using its current velocity.
movePaddle ::
  -- | The number of seconds since last update
  Float ->
  -- | The initial game state
  PongGame ->
  -- | A new game state with an updated ball position
  PongGame
movePaddle seconds game
  | S.member (Char 'j') (keys game) =
      if paddleWallCollisionTop $ player1 game
        then -- Do nothing
          game
        else -- Update position
          game {player1 = player1 game - 400 * seconds}
  | S.member (Char 'k') (keys game) =
      if paddleWallCollisionBottom $ player1 game
        then -- Do nothing
          game
        else -- Update position
          game {player1 = player1 game + 400 * seconds}
  | otherwise = game

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

-- | Exit the program if the ball is out of bounds or return PongGame
ballOutOfBounds :: PongGame -> PongGame
ballOutOfBounds game
  | ballX + radius < -(paddleDistance + paddleThickness) = error "You lost, THIS IS NOT A CRASH THIS IS NOT A CRASH THIS IS NOT A CRASH, i literally do not know how to make this program exit because i dont understand IO types"
  | otherwise = game
  where
    (ballX, _) = ballLoc game

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game {ballVel = (vx', vy)}
  where
    -- The old velocities.
    (vx, vy) = ballVel game

    vx' =
      if paddleCollision game
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

-- | Takes an x, min and max value, and checks if x fits within the range
inRange2 :: (Ord a) => a -> (a, a) -> Bool
inRange2 x (min', max') = x >= min' && x <= max'

-- | Given position and radius of the ball and the distance between paddles, return whether a collision occurred.
paddleCollision :: PongGame -> Bool
paddleCollision game = leftCollision || rightCollision
  where
    (ballX, ballY) = ballLoc game
    paddleY = player1 game
    leftCollision =
      ( inRange2
          (ballY + radius)
          (paddleY - paddleLength / 2, paddleY + paddleLength / 2)
          || inRange2
            (ballY - radius)
            (paddleY - paddleLength / 2, paddleY + paddleLength / 2)
      )
        && (ballX - radius <= -(paddleDistance - paddleThickness / 2))
    rightCollision = ballX + radius >= (paddleDistance - wallThickness / 2)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    topCollision = y - radius <= -(wallDistance - wallThickness / 2)
    bottomCollision = y + radius >= wallDistance - wallThickness / 2

-- | Given position and length of the paddle, return whether a top collision occurred.
paddleWallCollisionTop :: Float -> Bool
paddleWallCollisionTop paddleY = topCollision
  where
    topCollision = paddleY - paddleLength / 2 <= -(wallDistance - wallThickness / 2)

-- | Given position and length of the paddle, return whether a bottom collision occurred.
paddleWallCollisionBottom :: Float -> Bool
paddleWallCollisionBottom paddleY = bottomCollision
  where
    bottomCollision = paddleY + paddleLength / 2 >= wallDistance - wallThickness / 2

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- | Move down on j.
handleKeys (EventKey (Char 'j') Down _ _) game =
  game {keys = S.insert (Char 'j') (keys game)}
handleKeys (EventKey (Char 'j') Up _ _) game =
  game {keys = S.delete (Char 'j') (keys game)}
-- \| Move up on k.
handleKeys (EventKey (Char 'k') Down _ _) game =
  game {keys = S.insert (Char 'k') (keys game)}
handleKeys (EventKey (Char 'k') Up _ _) game =
  game {keys = S.delete (Char 'k') (keys game)}
-- Do nothing for all other events.
handleKeys _ game = game
