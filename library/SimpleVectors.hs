{-# LANGUAGE TypeFamilies #-}

module SimpleVectors where

import Data.VectorSpace

data V0 a = V0 deriving (Eq, Ord, Read, Show)
data V1 a = V1 a deriving (Eq, Ord, Read, Show)
data V2 a = V2 a a deriving (Eq, Ord, Read, Show)
data V3 a = V3 a a a deriving (Eq, Ord, Read, Show)
data V4 a = V4 a a a a deriving (Eq, Ord, Read, Show)

instance AdditiveGroup a => AdditiveGroup (V0 a) where
    zeroV = V0
    negateV V0 = V0
    V0 ^+^ V0 = V0
instance AdditiveGroup a => AdditiveGroup (V1 a) where
    zeroV = V1 zeroV
    negateV (V1 x) = V1 (negateV x)
    V1 x1 ^+^ V1 x2 = V1 (x1 ^+^ x2)
instance AdditiveGroup a => AdditiveGroup (V2 a) where
    zeroV = V2 zeroV zeroV
    negateV (V2 x y) = V2 (negateV x) (negateV y)
    V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 ^+^ x2) (y1 ^+^ y2)
instance AdditiveGroup a => AdditiveGroup (V3 a) where
    zeroV = V3 zeroV zeroV zeroV
    negateV (V3 x y z) = V3 (negateV x) (negateV y) (negateV z)
    V3 x1 y1 z1 ^+^ V3 x2 y2 z2 = V3 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2)
instance AdditiveGroup a => AdditiveGroup (V4 a) where
    zeroV = V4 zeroV zeroV zeroV zeroV
    negateV (V4 x y z w) = V4 (negateV x) (negateV y) (negateV z) (negateV w)
    V4 x1 y1 z1 w1 ^+^ V4 x2 y2 z2 w2 =
        V4 (x1 ^+^ x2) (y1 ^+^ y2) (z1 ^+^ z2) (w1 ^+^ w2)

instance VectorSpace a => VectorSpace (V0 a) where
    type Scalar (V0 a) = Scalar a
    _ *^ V0 = V0
instance VectorSpace a => VectorSpace (V1 a) where
    type Scalar (V1 a) = Scalar a
    a *^ V1 x = V1 (a *^ x)
instance VectorSpace a => VectorSpace (V2 a) where
    type Scalar (V2 a) = Scalar a
    a *^ V2 x y = V2 (a *^ x) (a *^ y)
instance VectorSpace a => VectorSpace (V3 a) where
    type Scalar (V3 a) = Scalar a
    a *^ V3 x y z = V3 (a *^ x) (a *^ y) (a *^ z)
instance VectorSpace a => VectorSpace (V4 a) where
    type Scalar (V4 a) = Scalar a
    a *^ V4 x y z w = V4 (a *^ x) (a *^ y) (a *^ z) (a *^ w)
