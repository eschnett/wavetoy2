{-# LANGUAGE ScopedTypeVariables #-}

module ManifoldSpec (spec) where

import Data.Bool
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Manifold



spec :: Spec
spec = parallel $ do
         describe "Interval1" specInterval1
         describe "ManifoldProduct" specManifoldProduct
         describe "ManifoldSum" specManifoldSum

emptyI1 :: Interval1 Double
emptyI1 = Interval1 1 0

specInterval1 :: Spec
specInterval1 = do
  it "has no points when empty" $
     property $ \(xs :: [Double]) ->
     not $ any (mfvalid emptyI1) xs
  it "has only points within bounds" $
     property $ \lo hi (xs :: [Double]) ->
     let m = Interval1 lo hi :: Interval1 Double
         inBounds x = lo <= x && x <= hi
     in all (\x -> mfvalid m x == inBounds x) xs
  -- it "has only points within bounds" $
  --    property $ \(m :: Interval1 Double) (xs :: [Double]) ->
  --    let (lo, hi) = bounds m ()
  --        inBounds x = lo <= x && x <= hi
  --    in all (\x -> mfvalid m x == inBounds x) xs
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

specManifoldProduct :: Spec
specManifoldProduct = do
  it "has no points when empty" $
     property $ \(m :: Interval1 Double) (xs :: [(Double, Double)]) ->
     let mp0 = ManifoldProduct emptyI1 emptyI1
         mp1 = ManifoldProduct m emptyI1
         mp2 = ManifoldProduct emptyI1 m
     in not (any (mfvalid mp0) xs) &&
        not (any (mfvalid mp1) xs) &&
        not (any (mfvalid mp2) xs)
  it "has only points within bounds" $
     property $ \(m1 :: Interval1 Double)
                 (m2 :: Interval1 Double)
                 (xs :: [(Double, Double)]) ->
     let m = ManifoldProduct m1 m2
     in all (\x -> mfvalid m x == (mfvalid m1 (fst x) && mfvalid m2 (snd x))) xs
  it "has the correct bounds" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldProduct m1 m2
     in bounds m (Left ()) == bounds m1 () &&
        bounds m (Right ()) == bounds m2 ()
  it "has a non-negative volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldProduct m1 m2
     in volume m >= 0 && (volume m1 > 0 && volume m2 > 0) >= (volume m > 0)
  it "has the correct volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldProduct m1 m2
     in volume m == volume m1 * volume m2

specManifoldSum :: Spec
specManifoldSum = do
  it "has no points when empty" $
     property $ \(xs :: [Either Double Double]) ->
     let mp = ManifoldSum emptyI1 emptyI1
     in not (any (mfvalid mp) xs)
  it "has only points within bounds" $
     property $ \(m1 :: Interval1 Double)
                 (m2 :: Interval1 Double)
                 (xs :: [Either Double Double]) ->
     let m = ManifoldSum m1 m2
     in all (\x -> mfvalid m x == either (mfvalid m1) (mfvalid m2) x) xs
  it "has the correct bounds" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldSum m1 m2
     in bounds m (Left ()) == bounds m1 () &&
        bounds m (Right ()) == bounds m2 ()
  it "has a non-negative volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldSum m1 m2
     in volume m >= 0 && (volume m1 > 0 || volume m2 > 0) >= (volume m > 0)
  it "has the correct volume" $
     property $ \(m1 :: Interval1 Double) (m2 :: Interval1 Double) ->
     let m = ManifoldSum m1 m2
     in volume m == volume m1 + volume m2
