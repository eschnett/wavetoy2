{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module FieldSpec (spec) where

import Control.Comonad
import Data.Fixed
import Data.VectorSpace
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Field
import Manifold

import IEEEUtils

default (Int)



spec :: Spec
spec = parallel $ do
         describe "PiecewiseLinearField1D" specPiecewiseLinearField1D
         describe "FieldProduct" specFieldProduct
         describe "FieldCompose" specFieldCompose



specPiecewiseLinearField1D :: Spec
specPiecewiseLinearField1D = do
  it "has the correct value when evaluated (linear)" $
     property $ \(m :: Interval1 Double) np' (xs' :: [Double]) ->
     let np = abs np'
         (lo, hi) = bounds m ()
         u = coordinatePiecewiseLinearField1D m np
         proj x = lo + mod' x (hi - lo)
         xs | mfempty m = []
            | mfdiscrete m () = [lo]
            | otherwise = [lo, hi] ++ map proj xs'
         val x | np == 0 = 0
               | np == 1 = lo
               | otherwise = x
         scale = absmaximum [lo, hi]
     in all (\x -> approxEq scale (evaluate x u) (val x)) xs
  it "approximates the correct value when evaluated (non-linear)" $
     property $ \(xs' :: [Double]) ->
     let np = 100
         (lo, hi) = (0, 1)
         m = Interval1 lo hi :: Interval1 Double
         f = samplePiecewiseLinearField1D m np sin
         proj x = lo + mod' x (hi - lo)
         xs = [lo, hi] ++ map proj xs'
         tol = 1 / fromIntegral np
     in all (\x -> approxEq' tol (evaluate x f) (sin x)) xs
  it "has the correct value when integrated" $
     property $ \(m :: Interval1 Double) np' ->
     let np = abs np'
         (lo, hi) = bounds m ()
         u = coordinatePiecewiseLinearField1D m np
         val | mfdiscrete m () || np == 0 = 0
             | np == 1 = lo * (hi - lo)
             | otherwise = 1/2 * (hi^2 - lo^2)
         scale = absmaximum [lo^2, hi^2]
     in approxEq scale (integral u) val
  it "has the correct volume" $
     property $ \(m :: Interval1 Double) np' ->
     let np = abs np'
         (lo, hi) = bounds m ()
         u = generatePiecewiseLinearField1D m (replicate np 1)
         val = if np == 0 then 0
               else volume m
         scale = absmaximum [lo, hi]
     in approxEq scale (integral u) val
  it "has the correct derivative" $
     property $ \(m :: Interval1 Double) np' (xs' :: [Double]) ->
     let np = abs np'
         (lo, hi) = bounds m ()
         u = coordinatePiecewiseLinearField1D m np
         du = extend (derivative ()) u
         h = (hi - lo) / fromIntegral (np - 1)
         proj x = lo + mod' x (hi - lo)
         xs | mfempty m = []
            | mfdiscrete m () = [lo]
            | otherwise = [lo, hi] ++ map proj xs'
         val | mfdiscrete m () || np <= 1 = 0
             | otherwise = 1
         scale = absmaximum [lo, hi, lo / h, hi / h]
     in all (\x -> approxEq scale (evaluate x du) val) xs
  it "has the correct boundary value" $
     property $ \(m :: Interval1 Double) np' (xs' :: [Double]) ->
     let np = abs np'
         (lo, hi) = bounds m ()
         u = coordinatePiecewiseLinearField1D m np
         bu = extend (boundary ()) u
         h = (hi - lo) / fromIntegral (np - 1)
         proj x = lo + mod' x (hi - lo)
         xs | mfempty m = []
            | mfdiscrete m () = [lo]
            | otherwise = [lo, hi] ++ map proj xs'
         good x val
             | mfdiscrete m () || np <= 1 = val == 0
             | approxEq scale x lo = approxEq scale val (-2*lo/h)
             | approxEq scale x hi = approxEq scale val (2*hi/h)
             | approxGt scale x (lo+h) && approxLt scale x (hi-h) = val == 0
             | otherwise = True
         scale = absmaximum [lo, hi, lo / h, hi / h]
     in all (\x -> good x (evaluate x bu)) xs
  it "has the correct boundary value" $
     -- TODO: Does this fail randomly? maybe lo' and hi' are cutting
     -- it too close?
     property $ \lo hi np (xs' :: [Double]) -> lo < hi && np > 3 ==>
     let m = Interval1 lo hi :: Interval1 Double
         u = coordinatePiecewiseLinearField1D m np
         bu = extend (boundary ()) u
         h = (hi - lo) / fromIntegral (np - 1)
         (lo', hi') = (lo + h, hi - h)
         xs = map (\x -> lo' + (if np > 3 then mod' x (hi' - lo') else 0)) xs'
         scale = absmaximum [lo, hi, 1 / (hi - lo)]
     in approxEq scale (evaluate lo bu) (-2*lo/h) &&
        approxEq scale (evaluate hi bu) (2*hi/h) &&
        all (\x -> approxEq scale (evaluate x bu) 0) xs
  it "has the correct boundary normal" $
     property $ \lo hi np (xs' :: [Double]) -> lo < hi && np > 3 ==>
     let m = Interval1 lo hi :: Interval1 Double
         u = coordinatePiecewiseLinearField1D m np
         bn = extend (boundaryNormal ()) u
         h = (hi - lo) / fromIntegral (np - 1)
         (lo', hi') = (lo + h, hi - h)
         xs = map (\x -> lo' + (if np > 3 then mod' x (hi' - lo') else 0)) xs'
         scale = absmaximum [lo, hi, 1 / (hi - lo)]
     in approxEq scale (evaluate lo bn) (-1/h) &&
        approxEq scale (evaluate hi bn) (1/h) &&
        all (\x -> approxEq scale (evaluate x bn) 0) xs
  it "satisfies Summation By Parts" $
     property $ \lo hi (us :: [(Double, Double)]) ->
     lo < hi && length us >= 2 ==>
     let m = Interval1 lo hi :: Interval1 Double
         u = generatePiecewiseLinearField1D m (map fst us)
         v = generatePiecewiseLinearField1D m (map snd us)
         du = extend (derivative ()) u
         dv = extend (derivative ()) v
         bu = extend (boundary ()) u
         bv = extend (boundary ()) v
         scale1 = absmaximum ([lo, hi] ++
                              values u ++ values du ++ values bu ++
                              values v ++ values dv ++ values bv)
         scale = absmaximum
                 [scale1, scale1 / (hi - lo), scale1^2, scale1^2 / (hi - lo)]
     in approxEq scale (du <.> v + u <.> dv) (u <.> bv) &&
        approxEq scale (du <.> v + u <.> dv) (bu <.> v)



specFieldProduct :: Spec
specFieldProduct = do
  it "has the correct value when evaluated (linear)" $
     property $ \(f1 :: PiecewiseLinearField1D Double Double)
                 (f2 :: PiecewiseLinearField1D Double Double)
                 (xs' :: [Either Double Double]) ->
                 -- TOOD: Remove this restriction
                 uncurry (<) (bounds (manifold_ f1) ()) &&
                 uncurry (<) (bounds (manifold_ f2) ()) ==>
     let f = fieldProduct f1 f2
         (lo1, hi1) = bounds (manifold_ f1) ()
         (lo2, hi2) = bounds (manifold_ f2) ()
         proj (Left x1) = Left $ lo1 + mod' x1 (hi1 - lo1)
         proj (Right x2) = Right $ lo2 + mod' x2 (hi2 - lo2)
         xs = [Left lo1, Left hi1, Right lo2, Right hi2] ++ map proj xs'
         eval1 x1 = evaluate x1 f1
         eval2 x2 = evaluate x2 f2
         scale = absmaximum [lo1, hi1, lo2, hi2]
     in all (\x -> approxEq scale (evaluate x f) (either eval1 eval2 x)) xs
  it "approximates the correct value when evaluated (non-linear)" $
     property $ \(xs' :: [Either Double Double]) ->
     let np1 = 40
         np2 = 60
         (lo1, hi1) = (0, 1)
         (lo2, hi2) = (1, 2)
         m1 = Interval1 lo1 hi1 :: Interval1 Double
         m2 = Interval1 lo2 hi2 :: Interval1 Double
         f1 = samplePiecewiseLinearField1D m1 np1 sin
         f2 = samplePiecewiseLinearField1D m2 np2 sin
         f = fieldProduct f1 f2
         proj (Left x1) = Left $ lo1 + mod' x1 (hi1 - lo1)
         proj (Right x2) = Right $ lo2 + mod' x2 (hi2 - lo2)
         xs = [Left lo1, Left hi1, Right lo2, Right hi2] ++ map proj xs'
         tol = max (1 / fromIntegral np1) (1 / fromIntegral np2)
     in all (\x -> approxEq' tol (evaluate x f) (either sin sin x)) xs
  -- it "has the correct value when integrated" $
  --    property $ \lo hi np -> lo < hi && np >= 2 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = coordinatePiecewiseLinearField1D m np
  --        scale = absmaximum [lo^2, hi^2]
  --    in approxEq scale (integral u) (1/2 * (hi^2 - lo^2))
  -- it "has the correct volume" $
  --    property $ \lo hi np -> lo < hi && np >= 2 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = generatePiecewiseLinearField1D m (replicate np 1)
  --        scale = absmaximum [lo, hi]
  --    in approxEq scale (integral u) (hi - lo)
  -- it "has the correct derivative" $
  --    property $ \lo hi np (xs' :: [Double]) -> lo < hi && np >= 2 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = coordinatePiecewiseLinearField1D m np
  --        du = extend (derivative ()) u
  --        xs = [lo, hi] ++ map (\x -> lo + mod' x (hi - lo)) xs'
  --        scale = absmaximum [lo, hi, 1 / (hi - lo)]
  --    in all (\x -> approxEq scale (evaluate x du) 1) xs
  -- it "has the correct boundary value" $
  --    property $ \lo hi np (xs' :: [Double]) -> lo < hi && np >= 3 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = coordinatePiecewiseLinearField1D m np
  --        bu = extend (boundary ()) u
  --        h = (hi - lo) / fromIntegral (np - 1)
  --        (lo', hi') = (lo + h, hi - h)
  --        xs = map (\x -> lo' + (if np > 3 then mod' x (hi' - lo') else 0)) xs'
  --        scale = absmaximum [lo, hi, 1 / (hi - lo)]
  --    in approxEq scale (evaluate lo bu) (-2*lo/h) &&
  --       approxEq scale (evaluate hi bu) (2*hi/h) &&
  --       all (\x -> approxEq scale (evaluate x bu) 0) xs
  -- it "has the correct boundary normal" $
  --    property $ \lo hi np (xs' :: [Double]) -> lo < hi && np >= 3 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = coordinatePiecewiseLinearField1D m np
  --        bn = extend (boundaryNormal ()) u
  --        h = (hi - lo) / fromIntegral (np - 1)
  --        (lo', hi') = (lo + h, hi - h)
  --        xs = map (\x -> lo' + (if np > 3 then mod' x (hi' - lo') else 0)) xs'
  --        scale = absmaximum [lo, hi, 1 / (hi - lo)]
  --    in approxEq scale (evaluate lo bn) (-1/h) &&
  --       approxEq scale (evaluate hi bn) (1/h) &&
  --       all (\x -> approxEq scale (evaluate x bn) 0) xs
  -- it "satisfies Summation By Parts" $
  --    property $ \lo hi (us :: [(Double, Double)]) ->
  --    lo < hi && length us >= 2 ==>
  --    let m = Interval1 lo hi :: Interval1 Double
  --        u = generatePiecewiseLinearField1D m (map fst us)
  --        v = generatePiecewiseLinearField1D m (map snd us)
  --        du = extend (derivative ()) u
  --        dv = extend (derivative ()) v
  --        bu = extend (boundary ()) u
  --        bv = extend (boundary ()) v
  --        scale1 = absmaximum ([lo, hi] ++
  --                             values u ++ values du ++ values bu ++
  --                             values v ++ values dv ++ values bv)
  --        scale = absmaximum $ [scale1, scale1 / (hi - lo),
  --                              scale1^2, scale1^2 / (hi - lo)]
  --    in approxEq scale (du <.> v + u <.> dv) (u <.> bv) &&
  --       approxEq scale (du <.> v + u <.> dv) (bu <.> v)



specFieldCompose :: Spec
specFieldCompose = do
  it "has the correct value when evaluated" $
     property $ \(m1 :: Interval1 Double)
                 (m2 :: Interval1 Double)
                 (vs12 :: [(Double, [Double])])
                 (xs' :: [(Double, Double)]) ->
                 -- TOOD: Remove this restriction
                 uncurry (<) (bounds m1 ()) &&
                 uncurry (<) (bounds m2 ()) &&
                 length vs12 > 0 &&
                 all (\(_, v2s) -> length v2s > 0) vs12 ==>
     let f2s = [generatePiecewiseLinearField1D m2 vs2 | vs2 <- map snd vs12]
         f12 = generatePiecewiseLinearField1D m1 f2s
         (lo1, hi1) = bounds m1 ()
         (lo2, hi2) = bounds m2 ()
         f = fieldCompose f12
         proj (x1, x2) = (lo1 + mod' x1 (hi1 - lo1), lo2 + mod' x2 (hi2 - lo2))
         xs = [(lo1, lo2), (lo1, hi2), (hi1, lo2), (hi1, hi2)] ++ map proj xs'
         eval12 (x1, x2) = evaluate x2 (evaluate x1 f12)
         scale = absmaximum [lo1, hi1, lo2, hi2]
     in all (\x -> approxEq scale (evaluate x f) (eval12 x)) xs
  it "approximates the correct value when evaluated (non-linear)" $
     property $ \(xs' :: [(Double, Double)]) ->
     let np1 = 100
         np2 = 100
         (lo1, hi1) = (0, 1)
         (lo2, hi2) = (0, 1)
         m1 = Interval1 lo1 hi1 :: Interval1 Double
         m2 = Interval1 lo2 hi2 :: Interval1 Double
         f12 = samplePiecewiseLinearField1D m1 np1 $ \x ->
               samplePiecewiseLinearField1D m2 np2 $ \y -> sin x * cos y
         f = fieldCompose f12
         proj (x1, x2) = (lo1 + mod' x1 (hi1 - lo1), lo2 + mod' x2 (hi2 - lo2))
         xs = [(lo1, lo2), (lo1, hi2), (hi1, lo2), (hi1, hi2)] ++ map proj xs'
         tol = max (1 / fromIntegral np1) (1 / fromIntegral np2)
     in all (\x -> approxEq' tol (evaluate x f) (sin (fst x) * cos (snd x))) xs
