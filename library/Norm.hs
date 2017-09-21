{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Norm where

import Test.QuickCheck

default (Int)



-- | L2Norm implements the algebra for calculating L2 norms
data Norm a = Norm { sumabs_, sumabssq_, maxabs_ :: a }
    deriving (Eq, Ord, Read, Show)

makeNorm :: Num a => a -> Norm a
makeNorm x = Norm (abs x) (abs x ^ 2) (abs x)

getL1Norm :: Norm a -> a
getL1Norm (Norm sm _ _) = sm

getL2Norm :: Floating a => Norm a -> a
getL2Norm (Norm _ ssq _) = sqrt ssq

getLInfNorm :: Norm a -> a
getLInfNorm (Norm _ _ mx) = mx

instance (Num a, Ord a) => Monoid (Norm a) where
    mempty = Norm 0 0 0
    mappend (Norm sm1 ssq1 mx1) (Norm sm2 ssq2 mx2) =
        Norm (sm1 + sm2) (ssq1 + ssq2) (max mx1 mx2)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Norm a) where
    arbitrary = do xs <- sized vector
                   return $ foldMap makeNorm xs
