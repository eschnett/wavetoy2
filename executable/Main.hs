{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Control.Monad
import Control.Monad.Loops
import Data.VectorSpace
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import System.IO
import Text.Printf

import Cell
import Field
import WaveToy2

-- Parameters
tini :: Double
tini = 0

xmin :: Double
xmin = 0

xmax :: Double
xmax = 1

npoints :: Int
npoints = 21

dx :: Double
dx = (xmax - xmin) / fromIntegral (npoints - 1)

alpha :: Double
alpha = 1 / 2

dt :: Double
dt = alpha * dx

niters :: Int
niters = round (fromIntegral (npoints - 1) / alpha)

outInfoEvery :: Int
outInfoEvery = niters `div` 10

outFileEvery :: Int
outFileEvery = niters `div` 10

outGraphEvery :: Int
outGraphEvery = niters `div` 10

iterateWhileM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateWhileM_ predicate action state = do
    _ <- iterateUntilM (not . predicate) action state
    return ()

main :: IO ()
main = do
    putStrLn "WaveToy2"
    let skel = skeletonState (xmin, xmax) npoints
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

output :: (InnerSpace a, PlotValue a, PrintfArg a, RealFloat a, Show a,
           Floating (Scalar a), Show (Scalar a)) =>
          (Int, State a (Cell a)) -> IO ()
output (iter, state) = do
  when (iter == niters || iter `mod` outInfoEvery == 0) $
       do outputInfo (iter, state)
  when (iter == niters || iter `mod` outFileEvery == 0) $
       do outputFile (iter, state)
  when (iter == niters || iter `mod` outGraphEvery == 0) $
       do outputGraph (iter, state)

outputInfo :: (RealFloat a, InnerSpace a, Show a,
               Floating (Scalar a), Show (Scalar a)) =>
              (Int, State a (Cell a)) -> IO ()
outputInfo (iter, state) = do
  putStrLn $ "iteration: " ++ show iter
  putStrLn $ "  time: " ++ show (time state)
  let energy = integralState $ energyState state
  putStrLn $ "  energy: " ++ show energy
  let error = normState (errorState state)
  putStrLn $ "  L2 error: " ++ show error

outputFile :: (RealFloat a, InnerSpace a, Show a,
               Floating (Scalar a), Show (Scalar a)) =>
              (Int, State a (Cell a)) -> IO ()
outputFile (iter, state) =
  withFile "wavetoy.dat" mode $ \h -> do
    hPutStrLn h $ ""
    hPutStrLn h $ "# iteration: " ++ show iter
    hPutStrLn h $ "# time: " ++ show (time state)
    hPutStrLn h $ "# x u rho vx"
    hPutStrLn h $ showField (cells state)
    let energy = integralState $ energyState state
    hPutStrLn h $ "# energy: " ++ show energy
    let error = normState (errorState state)
    hPutStrLn h $ "# L2 error: " ++ show error
    where mode = if iter==0 then WriteMode else AppendMode
          showVal v = show v ++ " "
          showCell c = concat (fmap showVal [u c, rho c, vx c]) ++ "\n"
          showField f = concat (fmap showCell (values f))

outputGraph :: (PlotValue a, PrintfArg a, RealFloat a, Show a) =>
               (Int, State a (Cell a)) -> IO ()
outputGraph (iter, state) =
    toFile def filename $ do
      layout_title .= printf "WaveToy2 t=%g" (time state)
      layout_y_axis . laxis_generate .= scaledAxis def (-1.1, 1.1)
      setColors [opaque blue, opaque red]
      plot $ line "u(x)" [pts]
      plot $ points "u(x) points" pts
    where filename = printf "wavetoy-i%04d.svg" iter
          xs = values (cells (coordState state))
          us = [u c | c <- values (cells state)]
          pts = zip xs us
