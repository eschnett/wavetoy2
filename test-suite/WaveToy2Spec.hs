module WaveToy2Spec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import WaveToy2

import IEEEUtils



spec = ()
{-
spec :: Spec
spec = do
  describe "skeletonState" $ do
         it "has the right extent" $
            property $ \xmin xmax np -> (np > 3 && xmax > xmin) ==>
                let skel = skeletonState (xmin, xmax) np :: State Double ()
                in bnds skel == (xmin, xmax)
         it "has the right size" $
            property $ \xmin xmax np -> (np > 3 && xmax > xmin) ==>
                let skel = skeletonState (xmin, xmax) np :: State Double ()
                in length (cells skel) == np
  describe "coordState" $ do
         it "is monotonic" $
            property $ \xmin xmax np -> (np > 3 && xmax > xmin) ==>
                let skel = skeletonState (xmin, xmax) np
                    coords = coordState skel :: State Double Double
                    cs = cells coords
                in all id [cs ! i > cs ! (i - 1) | i <- [1 .. V.length cs - 1]]
        describe "integralState" $ do
            it "is linear" $
                property $ \alpha beta xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel :: State Double Double
                        int =
                            integralState $ fmap (\x -> alpha * x + beta) coords
                        good_int =
                            1 / 2 * alpha * (xmax ^ 2 - xmin ^ 2) +
                            beta * (xmax - xmin)
                        scale =
                            absmaximum
                                [ alpha
                                , beta
                                , xmin
                                , xmax
                                , xmin ^ 2
                                , xmax ^ 2
                                , alpha * xmin ^ 2
                                , alpha * xmax ^ 2
                                , beta * xmin
                                , beta * xmax
                                ]
                    in approxEq scale int good_int
        describe "normState" $ do
            it "is positive" $
                property $ \alpha xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel :: State Double Double
                        g = fmap (\x -> Identity (alpha * x)) coords
                        norm = normState g
                    in norm >= 0
            it "is linear" $
                property $ \beta xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel :: State Double Double
                        g = fmap (const $ Identity beta) coords
                        norm = normState g :: Double
                        scale = absmaximum [epsilon, beta ^ 2]
                    in approxEq scale norm (abs beta)
        describe "initState" $ do
            it "has the right time" $
                property $ \t xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                    in time g == t
        describe "errorState" $ do
            it "is zero when exact" $
                property $ \t xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                        err = errorState g
                    in all (all (~~ 0)) err
            it "is not all zero when inexact" $
                property $ \t t' xmin xmax np ->
                    (t /= t' && np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                        g' = initState t' coords
                        err = errorState (g {cells = cells g'})
                    in not $ all (all (~~ 0)) err
        describe "energyState" $ do
            it "is positive" $
                property $ \t xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                        etot = integralState $ energyState g
                    in etot > 0
        describe "rhsState" $ do
            it "does stuff" $
                property $ \t xmin xmax np ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                        r = bcState (rhsState g)
                        norm = normState r
                    in norm >= 0
        describe "rk2State" $ do
            it "does stuff" $
                property $ \t xmin xmax np dt ->
                    (np > 3 && xmax > xmin) ==>
                    let skel = skeletonState (xmin, xmax) np
                        coords = coordState skel
                        g = initState t coords :: State Double (Cell Double)
                        rhs g = bcState (rhsState g)
                        g' = rk2State dt rhs g
                        norm = normState g'
                    in norm >= 0
-}
