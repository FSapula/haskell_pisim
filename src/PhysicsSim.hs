module PhysicsSim (
  simulationStep,
  Box (..),
  Simulation (..),
  module Control.Monad.State,
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
  }
  deriving (Show)
data NextCollision = Boxes Double | Wall Double deriving (Eq, Ord, Show)
type Time = Double

timeToWall :: Simulation -> Maybe Double
timeToWall (Sim box _)
  | velocity box >= 0 = Nothing
  | otherwise = Just $ abs $ (position box - width box) / velocity box

timeToBox :: Simulation -> Maybe Double
timeToBox (Sim box1 box2)
  | velocity box2 >= velocity box1 = Nothing
  | otherwise = return $ dist / relvelocity
 where
  dist = (position box2 - width box2) - (position box1 + width box1)
  relvelocity = velocity box1 - velocity box2

getNextCollision :: Simulation -> Maybe NextCollision
getNextCollision sim = min <$> wallT <*> boxT <|> wallT <|> boxT
 where
  wallT = Wall <$> timeToWall sim
  boxT = Boxes <$> timeToBox sim

boxMove :: Time -> Simulation -> Simulation
boxMove t (Sim box1 box2) = Sim new1 new2
 where
  new1 = box1{position = position box1 + velocity box1 * t}
  new2 = box2{position = position box2 + velocity box2 * t}

calcCollisionSpeed :: Simulation -> Simulation
calcCollisionSpeed (Sim box1 box2) = Sim new1 new2
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
  flipBoxVel (Sim box1 box2) = Sim box1{velocity = (-1) * velocity box1} box2

collideBoxes :: Time -> State (Int, Simulation) ()
collideBoxes t = modify (\(i, s) -> (i + 1, newstate s))
 where
  newstate s = calcCollisionSpeed $ boxMove t s

simulationStep :: State (Int, Simulation) (String)
simulationStep = do
  (i, s) <- get
  case getNextCollision s of
    (Just (Wall t)) -> do
      collideWithWall t
      simulationStep
      return $ "Wall:" ++ show t
    (Just (Boxes t)) -> do
      collideBoxes t
      simulationStep
      return $ "Box:" ++ show t
    Nothing -> do
      return "End"
