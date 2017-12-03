{-# LANGUAGE ScopedTypeVariables #-}

module ChartSpec (spec) where

import Data.Bool
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Chart



spec :: Spec
spec = parallel $ do
         describe "Interval1" specInterval1
         describe "ChartProduct" specChartProduct
         describe "ChartSum" specChartSum

emptyI1 :: Interval1 Double
emptyI1 = Interval1 1 0

specInterval1 :: Spec
specInterval1 = do
  it "has no points when empty" $
     property $ \(xs :: [Double]) ->
     not $ any (valid emptyI1) xs
  it "has only points within bounds" $
     property $ \lo hi (xs :: [Double]) ->
     let m = Interval1 lo hi :: Interval1 Double
         inBounds x = lo <= x && x <= hi
     in all (\x -> valid m x == inBounds x) xs
  -- it "has only points within bounds" $
  --    property $ \(m :: Interval1 Double) (xs :: [Double]) ->
  --    let (lo, hi) = bounds m ()
  --        inBounds x = lo <= x && x <= hi
  --    in all (\x -> valid m x == inBounds x) xs
  it "has the correct bounds" $
     property $ \lo hi ->
     let m = Interval1 lo hi :: Interval1 Double
     in bounds m () == (lo, hi)
  it "has a non-negative volume" $
     property $ \lo hi ->
     let m = Interval1 lo hi :: Interval1 Double
     in volume m >= 0 && (lo < hi) >= (volume m > 0)
  it "has the correct volume" $
     property $ \lo hi ->
     let m = Interval1 lo hi :: Interval1 Double
     in volume m == bool (hi - lo) 0 (lo > hi)

specChartProduct :: Spec
specChartProduct = do
  it "has no points when empty" $
     property $ \(m :: Interval1 Double) (xs :: [(Double, Double)]) ->
     let mp0 = ChartProduct emptyI1 emptyI1
         mp1 = ChartProduct m emptyI1
         mp2 = ChartProduct emptyI1 m
     in not (any (valid mp0) xs) &&
        not (any (valid mp1) xs) &&
        not (any (valid mp2) xs)
  it "has only points within bounds" $
     property $ \(m1 :: Interval1 Double)
                 (m2 :: Interval1 Double)
                 (xs :: [(Double, Double)]) ->
     let m = ChartProduct m1 m2
     in all (\x -> valid m x == (valid m1 (fst x) && valid m2 (snd x))) xs
  it "has the correct bounds" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartProduct m1 m2
     in bounds m (Left ()) == bounds m1 () &&
        bounds m (Right ()) == bounds m2 ()
  it "has a non-negative volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartProduct m1 m2
     in volume m >= 0 && (volume m1 > 0 && volume m2 > 0) >= (volume m > 0)
  it "has the correct volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartProduct m1 m2
     in volume m == volume m1 * volume m2

specChartSum :: Spec
specChartSum = do
  it "has no points when empty" $
     property $ \(xs :: [Either Double Double]) ->
     let mp = ChartSum emptyI1 emptyI1
     in not (any (valid mp) xs)
  it "has only points within bounds" $
     property $ \(m1 :: Interval1 Double)
                 (m2 :: Interval1 Double)
                 (xs :: [Either Double Double]) ->
     let m = ChartSum m1 m2
     in all (\x -> valid m x == either (valid m1) (valid m2) x) xs
  it "has the correct bounds" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartSum m1 m2
     in bounds m (Left ()) == bounds m1 () &&
        bounds m (Right ()) == bounds m2 ()
  it "has a non-negative volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartSum m1 m2
     in volume m >= 0 && (volume m1 > 0 || volume m2 > 0) >= (volume m > 0)
  it "has the correct volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ChartSum m1 m2
     in volume m == volume m1 + volume m2
