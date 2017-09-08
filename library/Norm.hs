-- {-# LANGUAGE TypeFamilies #-}

module Norm where

-- import Data.VectorSpace

-- | L2Norm implements the algebra for calculating L2 norms
data Norm a = Norm { count_, sumabs_, sumsq_, maxabs_ :: a }
              deriving (Eq, Ord, Read, Show)

makeNorm :: Num a => a -> Norm a
makeNorm x = Norm 1 (abs x) (abs x ^ 2) (abs x)

getL1Norm :: Floating a => Norm a -> a
getL1Norm (Norm cnt sm _ _) = sm / cnt

getL2Norm :: Floating a => Norm a -> a
getL2Norm (Norm cnt _ ssq _) = sqrt (ssq / cnt)

getLInfNorm :: Norm a -> a
getLInfNorm (Norm _ _ _ mx) = mx

instance (Num a, Ord a) => Monoid (Norm a) where
    mempty = Norm 0 0 0 0
    mappend (Norm cnt1 sm1 ssq1 mx1) (Norm cnt2 sm2 ssq2 mx2) =
        Norm (cnt1 + cnt2) (sm1 + sm2) (ssq1 + ssq2) (max mx1 mx2)

-- -- This is not really a group...
-- instance AdditiveGroup a => AdditiveGroup (Norm a) where
--     zeroV = Norm zeroV zeroV zeroV
--     Norm cnt1 sm1 ssq1 ^+^ Norm cnt2 sm2 ssq2 =
--         Norm (cnt1 ^+^ cnt2) (sm1 ^+^ sm2) (ssq1 ^+^ ssq2)
--     -- negateV cannot be defined correctly
--     negateV (Norm cnt1 sm1 ssq1) = Norm (negateV cnt1) (negateV sm1) ssq1
-- 
-- instance VectorSpace a => VectorSpace (Norm a) where
--     type Scalar (Norm a) = Scalar a
--     alpha *^ Norm cnt1 sm1 ssq1 =
--         Norm (alpha *^ cnt1) (alpha *^ sm1) (alpha *^ ssq1)
