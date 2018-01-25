module Main(main) where

import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Random

type Radius = Float
type Position = (Float, Float)

ballRadius :: Radius
ballRadius = 20

-- | Data for the state of the game
data PongGame = Game
	{ ballLoc :: (Float, Float)
	, ballVel :: (Float, Float)
	, player0 :: Float

	, player1 :: Float
	} deriving Show

-- | Converts a game state into a picture.
render :: PongGame  
       -> IO Picture   
render game = return $
  pictures [ball, walls,
            mkPaddle red 120 $ player0 game,
            mkPaddle blue (-120) $ player1 game]
  where
    -- | The ball
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark magenta

    -- | Settings of bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 450 10

    wallColor = light ( light blue)
    walls = pictures [wall 150, wall (-150)]

    -- | Settings of a paddle with given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light green

-- | Updating the game while the ball is moving.
update :: Float -> PongGame -> IO PongGame
update seconds game = 
  if gameEnded game'
  then do 
  	putStrLn "Thus is the end!"
  	exitSuccess
  else return game'

  where
  	game' = paddleBounce . wallBounce . moveBall seconds $ game

-- | Checks whether the game has ended.
gameEnded :: PongGame -> Bool
gameEnded game = farLeft || farRight
  where
  	(x, _) = ballLoc game
  	farLeft = x < -fromIntegral width / 2 + 2 * ballRadius
  	farRight = x > fromIntegral width / 2 - 2 * ballRadius

-- | Key events.
handleKeys :: Event -> PongGame -> IO PongGame
handleKeys event game = case event of
  EventKey (Char 'x') _ _ _ -> exitSuccess
  EventKey (Char 'q') _ _ _ -> return $
    game { player1 = player1 game + 10 }
  EventKey (Char 'a') _ _ _ ->  return $
    game { player1 = player1 game - 10 }
  EventKey (SpecialKey KeyUp) _ _ _ ->  return $
    game { player0 = player0 game + 10 }
  EventKey (SpecialKey KeyDown) _ _ _ ->  return $
    game { player0 = player0 game - 10 }
  _ -> return game


-- | Returns if a collision occurs.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius  <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where

    -- Previous velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) ballRadius
          then 
             -- | Updating velocity.
             -vy
           else
            -- | Current state does not change. Return the previous velocity.
            vy

paddleCollision :: Position -> PongGame -> Bool
paddleCollision (x, y) game = 
  (x + ballRadius > 110 && abs (y - player0 game) < 40) || 
  (x - ballRadius < -110 && abs (y - player1 game) < 40)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- | Previous velocities.
    (vx, vy) = ballVel game
    vx' = if paddleCollision (ballLoc game) game then -vx else vx


moveBall :: Float
		 -> PongGame
		 -> PongGame
moveBall seconds game = game {ballLoc = (x', y') }
  where
  	--Previous locations and velocities
  	(x, y) = ballLoc game
  	(vx, vy) = ballVel game

  	--New locations
  	x' = x + vx * seconds
  	y' = y + vy * seconds

randomInitialState :: StdGen -> PongGame
randomInitialState gen = Game
  { ballLoc = (a, b)
  , ballVel = (c', d')
  , player0 = 0
  , player1 = 0
  }
  where
  	a:b:c:d:_ = randomRs (-50,50) gen
  	c' = c * mag
  	d' = d * mag
  	mag = 200 / sqrt (c^2 + d^2)

width, height, offset :: Num a => a
width = 800
height = 800
offset = 200

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

-- | Main
main :: IO()
main = do 
	gen <- getStdGen
	let initState = randomInitialState gen
	playIO window background fps initState render handleKeys update