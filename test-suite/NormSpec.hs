{-# LANGUAGE ScopedTypeVariables #-}

module NormSpec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import Norm

import IEEEUtils



spec :: Spec
spec = parallel $ do
         describe "L1Norm" specL1Norm
         describe "L2Norm" specL2Norm
         describe "LInfNorm" specLInfNorm

specL1Norm :: Spec
specL1Norm = do
      it "is zero when empty" $
         property $
         let n = mempty :: Norm Double
         in getL1Norm n == 0
      it "is the absolute value for a single value" $
         property $ \x ->
         let n = makeNorm x :: Norm Double
         in getL1Norm n == abs x
      it "is positive" $
         property $ \(xs :: [Double]) ->
         let n = getL1Norm $ foldMap makeNorm xs
         in n >= 0
      it "is definite" $
         property $ \(xs :: [Double]) ->
         let n = getL1Norm $ foldMap makeNorm xs
         in (n /= 0) == any (/= 0) xs
      it "is linear" $
         property $ \beta (xs :: [Double]) ->
         let n1 = getL1Norm $ foldMap makeNorm xs
             ys = fmap (* beta) xs
             n2 = getL1Norm $ foldMap makeNorm ys
             scale = absmaximum [1, beta]
         in approxEq scale n2 (abs beta * n1)
      it "is convex" $
         property $ \(xs :: [Double]) ->
         let n =  getL1Norm $ foldMap makeNorm xs
         in all (\x -> n >= abs x) xs
      it "is triangular" $
         property $ \(xs :: [(Double, Double)]) ->
         let n1 = getL1Norm $ foldMap makeNorm (map fst xs)
             n2 = getL1Norm $ foldMap makeNorm (map snd xs)
             n3 = getL1Norm $ foldMap makeNorm (map (uncurry (+)) xs)
             scale = absmaximum $ [1] ++ map (uncurry max) xs
         in approxLtEq scale n3 (n1 + n2)
      it "is correct for an example" $
         property $
         let xs = [1, 2, 3, 4, 5] :: [Double]
             n = foldMap makeNorm xs
         in getL1Norm n == 15

specL2Norm :: Spec
specL2Norm =
    describe "L2Norm" $ do
      it "is zero when empty" $
         property $
         let n = mempty :: Norm Double
         in getL2Norm n == 0
      it "is the absolute value for a single value" $
         property $ \(x :: Double) ->
         let n = makeNorm x
         in getL2Norm n == abs x
      it "is positive" $
         property $ \(xs :: [Double]) ->
         let n = getL2Norm $ foldMap makeNorm xs
         in n >= 0
      it "is definite" $
         property $ \(xs :: [Double]) ->
         let n = getL2Norm $ foldMap makeNorm xs
         in (n /= 0) == any (/= 0) xs
      it "is linear" $
         property $ \beta(xs :: [Double]) ->
         let n1 = getL2Norm $ foldMap makeNorm xs
             ys = fmap (* beta) xs
             n2 = getL2Norm $ foldMap makeNorm ys
             scale = absmaximum [1, beta]
         in approxEq scale n2 (abs beta * n1)
      it "is convex" $
         property $ \(xs :: [Double]) ->
         let n = getL2Norm $ foldMap makeNorm xs
         in all (\x -> n >= abs x) xs
      it "is triangular" $
         property $ \(xs :: [(Double, Double)]) ->
         let n1 = getL2Norm $ foldMap makeNorm (map fst xs)
             n2 = getL2Norm $ foldMap makeNorm (map snd xs)
             n3 = getL2Norm $ foldMap makeNorm (map (uncurry (+)) xs)
             scale = absmaximum $ [1] ++ map (uncurry max) xs
         in approxLtEq scale n3 (n1 + n2)
      it "is correct for an example" $
         property $
         let xs = [1, 2, 3, 4, 5] :: [Double]
             n = foldMap makeNorm xs
         in getL2Norm n ~~ sqrt 55

specLInfNorm :: Spec
specLInfNorm =
    describe "LInfNorm" $ do
      it "is zer0 when empty" $
         property $
         let n = mempty :: Norm Double
         in getLInfNorm n == 0
      it "is the absolute value for a single value" $
         property $ \(x :: Double) ->
         let n = makeNorm x
         in getLInfNorm n == abs x
      it "is positive" $
         property $ \(xs :: [Double]) ->
         let n = getLInfNorm $ foldMap makeNorm xs
         in n >= 0
      it "is definite" $
         property $ \(xs :: [Double]) ->
         let n = getLInfNorm $ foldMap makeNorm xs
         in (n /= 0) == any (/= 0) xs
      it "is linear" $
         property $ \beta (xs :: [Double]) ->
         let n1 = getLInfNorm $ foldMap makeNorm xs
             ys = fmap (* beta) xs
             n2 = getLInfNorm $ foldMap makeNorm ys
             scale = absmaximum [1, beta]
         in approxEq scale n2 (abs beta * n1)
      it "is convex" $
         property $ \(xs :: [Double]) ->
         let n = foldMap makeNorm xs
             r = getLInfNorm n
         in all (\x -> r >= abs x) xs
      it "is triangular" $
         property $ \(xs :: [(Double, Double)]) ->
         let n1 = getLInfNorm $ foldMap makeNorm (map fst xs)
             n2 = getLInfNorm $ foldMap makeNorm (map snd xs)
             n3 = getLInfNorm $ foldMap makeNorm (map (uncurry (+)) xs)
             scale = absmaximum $ [1] ++ map (uncurry max) xs
         in approxLtEq scale n3 (n1 + n2)
      it "is correct for an example" $
         property $
         let xs = [1, 2, 3, 4, 5] :: [Double]
             n = foldMap makeNorm xs
         in getLInfNorm n == 5
