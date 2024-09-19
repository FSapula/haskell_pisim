module PhysicsSim (
  Box (..),
  Simulation (..),
  module Control.Monad.State,
  runSimLog,
  boxMove,
  Time,
  SimulationLog,
  runSimNoLog,
) where

import Control.Applicative
import Control.Monad.State

data Box = Box
  { position :: Double
  , mass :: Int
  , velocity :: Double
  , width :: Double
  }
  deriving (Show)
data Simulation = Sim
  { box1 :: Box
  , box2 :: Box
  , time :: Time
  }
  deriving (Show)
data Collision = Boxes Double | Wall Double deriving (Eq, Ord, Show)
type Time = Double
type SimulationLog = [Simulation]

timeToWall :: Simulation -> Maybe Double
timeToWall (Sim box _ _)
  | velocity box >= 0 = Nothing
  | otherwise = return $ abs $ (position box - width box) / velocity box

timeToBox :: Simulation -> Maybe Double
timeToBox (Sim box1 box2 _)
  | velocity box2 >= velocity box1 = Nothing
  | otherwise = return $ dist / relvelocity
 where
  dist = (position box2 - width box2) - (position box1 + width box1)
  relvelocity = velocity box1 - velocity box2

getNextCollision :: Simulation -> Maybe Collision
getNextCollision sim = min <$> wallT <*> boxT <|> wallT <|> boxT
 where
  wallT = Wall <$> timeToWall sim
  boxT = Boxes <$> timeToBox sim

boxMove :: Time -> Simulation -> Simulation
boxMove t (Sim box1 box2 simt) = Sim new1 new2 (simt + t)
 where
  new1 = box1{position = position box1 + velocity box1 * t}
  new2 = box2{position = position box2 + velocity box2 * t}

calcCollisionSpeed :: Simulation -> Simulation
calcCollisionSpeed (Sim box1 box2 t) = Sim new1 new2 t
 where
  v1 = velocity box1
  v2 = velocity box2
  m1 = fromIntegral $ mass box1
  m2 = fromIntegral $ mass box2
  new1 = box1{velocity = nv1}
  new2 = box2{velocity = nv2}
  nv1 = (v1 * (m1 - m2) + 2 * m2 * v2) / (m1 + m2)
  nv2 = (v2 * (m2 - m1) + 2 * m1 * v1) / (m1 + m2)

collideWithWall :: Time -> State (Int, Simulation) ()
collideWithWall t = modify (\(i, s) -> (i + 1, newstate s))
 where
  newstate s = flipBoxVel (boxMove t s)
  flipBoxVel :: Simulation -> Simulation
  flipBoxVel (Sim box1 box2 t) = Sim box1{velocity = (-1) * velocity box1} box2 t

collideBoxes :: Time -> State (Int, Simulation) ()
collideBoxes t = modify (\(i, s) -> (i + 1, newstate s))
 where
  newstate s = calcCollisionSpeed $ boxMove t s

simStep :: Collision -> State (Int, Simulation) ()
simStep col = case col of
  (Wall t) -> collideWithWall t
  (Boxes t) -> collideBoxes t

runSimLog :: Simulation -> SimulationLog
runSimLog sim = runSimHelper sim []
 where
  runSimHelper :: Simulation -> SimulationLog -> SimulationLog
  runSimHelper sim simlog = do
    case getNextCollision sim of
      Nothing -> simlog
      (Just x) -> do
        let (_, newsim) = execState (simStep x) (0, sim)
        runSimHelper newsim (simlog ++ [newsim])

runSimNoLog :: Simulation -> Int
runSimNoLog sim = evalState (runSimHelper) (0, sim)
 where
  runSimHelper :: State (Int, Simulation) (Int)
  runSimHelper = do
    (i, s) <- get
    case getNextCollision s of
      Nothing -> return i
      (Just x) -> do
        simStep x
        runSimHelper
