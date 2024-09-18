module Lib (
  someFunc,
) where

import PhysicsSim

someFunc :: IO ()
someFunc = do
  let box1 = Box 1 1 0 0
  let box2 = Box 5 1000000 (-1) 0
  let sim = Sim box1 box2
  print $ runState simulationStep (0, (Sim box1 box2))
