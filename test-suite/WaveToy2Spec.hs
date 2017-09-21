{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module WaveToy2Spec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Cell
import Field
import Manifold
import SimpleVectors
import WaveToy2

import IEEEUtils

default (Int)



spec :: Spec
spec = parallel $ do
         describe "skeletonState" specSkeletonState
         describe "coordState" specCoordState
         describe "integralState" specIntegralState
         describe "normState" specNormState
         describe "initState" specInitState
         describe "errorState" specErrorState
         describe "energyState" specEnergyState
         describe "rhsState" specRhsState
         describe "rk2State" specRk2State



specSkeletonState :: Spec
specSkeletonState = do
  it "has the right extent" $
     property $ \xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np :: State Double (V0 Double)
     in bounds (getManifold (cells skel)) () == (xmin, xmax)
  it "has the right size" $
     property $ \xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np :: State Double (V0 Double)
     in length (cells skel) == np

specCoordState :: Spec
specCoordState = do
  it "is monotonic" $
     property $ \xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel :: State Double Double
         cs = values (cells coords)
     in and [cs !! i > cs !! (i - 1) | i <- [1 .. length cs - 1]]

specIntegralState :: Spec
specIntegralState = do
  it "is linear" $
     property $ \alpha beta xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel :: State Double Double
         int = integralState $ fmap (\x -> alpha * x + beta) coords
         good_int =
             1/2 * alpha * (xmax^2 - xmin^2) + beta * (xmax - xmin)
         scale = absmaximum
                 [ alpha
                 , beta
                 , xmin
                 , xmax
                 , xmin^2
                 , xmax^2
                 , alpha * xmin^2
                 , alpha * xmax^2
                 , beta * xmin
                 , beta * xmax
                 ]
     in approxEq scale int good_int

specNormState :: Spec
specNormState = do
  it "is positive" $
     property $ \alpha xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel :: State Double Double
         g = fmap (\x -> alpha * x) coords
         norm = normState g
     in norm >= 0
  it "is linear" $
     property $ \beta xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel :: State Double Double
         g = fmap (const beta) coords
         norm = normState g :: Double
         scale = absmaximum [1, beta ^ 2]
     in approxEq scale norm (abs beta)

specInitState :: Spec
specInitState = do
  it "has the right time" $
     property $ \t xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
     in time g == t

specErrorState :: Spec
specErrorState = do
  it "is zero when exact" $
     property $ \t xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
         err = errorState g
     in all (all (~~ 0)) err
  it "is not all zero when inexact" $
     property $ \t t' xmin xmax np ->
         t /= t' && np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
         g' = initState t' coords
         err = errorState (g {cells = cells g'})
     in not $ all (all (~~ 0)) err

specEnergyState :: Spec
specEnergyState = do
  it "is positive" $
     property $ \t xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
         etot = integralState $ energyState g
     in etot > 0

specRhsState :: Spec
specRhsState = do
  it "does stuff" $
     property $ \t xmin xmax np -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
         r = rhsState g
         norm = normState r
     in norm >= 0

specRk2State :: Spec
specRk2State = do
  it "does stuff" $
     property $ \t xmin xmax np dt -> np > 3 && xmax > xmin ==>
     let skel = skeletonState (xmin, xmax) np
         coords = coordState skel
         g = initState t coords :: State Double (Cell Double)
         g' = rk2State dt rhsState g
         norm = normState g'
     in norm >= 0
