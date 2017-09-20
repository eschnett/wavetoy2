{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Control.Monad
import Control.Monad.Loops
import Data.VectorSpace

import Cell
import WaveToy2

-- Parameters
tini :: Double
tini = 0

xmin :: Double
xmin = 0

xmax :: Double
xmax = 1

ncells :: Int
ncells = 20

dx :: Double
dx = (xmax - xmin) / fromIntegral ncells

alpha :: Double
alpha = 1 / 2

dt :: Double
dt = alpha * dx

niters :: Int
niters = round (fromIntegral ncells / alpha)

outEvery :: Int
outEvery = niters `div` 10

iterateWhileM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateWhileM_ predicate action state = do
    _ <- iterateUntilM (not . predicate) action state
    return ()

main :: IO ()
main = do
    putStrLn "WaveToy2"
    let skel = skeletonState (xmin, xmax) (ncells + 3)
    let iter = 0
    let state = initState tini skel
    output (iter, state)
    iterateWhileM_ cond step (iter, state)
  where
    cond (iter, state) = iter < niters
    step (iter, state) = do
        let iter' = iter + 1
        let state' = rk2State dt rhsState state
        output (iter', state')
        return (iter', state')

output :: ( InnerSpace a
          , RealFloat a
          , Show a
          , Floating (Scalar a)
          , Show (Scalar a)) =>
         (Int, State a (Cell a)) -> IO ()
output (iter, state) = do
    when (iter == niters || iter `mod` outEvery == 0) $ do
        putStrLn $ "iteration: " ++ show iter
        putStrLn $ "  time: " ++ show (time state)
        let energy = integralState $ energyState state
        putStrLn $ "  energy: " ++ show energy
        let error = normState (errorState state)
        putStrLn $ "  L2 error: " ++ show error
