{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Manifold
    ( Manifold(..)
    , ManifoldOk
    , ManifoldProduct(..)
    , ManifoldSum(..)
    , Interval1(..)
    ) where

import Data.Kind



-- | A manifold
class Manifold m where
    type Ok m :: Constraint
    type Ok m = ManifoldOk m
    type Dimension m :: Type
    type Point m :: Type
    type Coordinate m :: Type
    volume :: Ok m => m -> Coordinate m
    bounds :: Ok m => m -> Dimension m -> (Coordinate m, Coordinate m)
    isValid :: Ok m => m -> Point m -> Bool

type ManifoldOk m = RealFrac (Coordinate m)



-- | The product of two manifolds
data ManifoldProduct m n = ManifoldProduct m n
                           deriving (Eq, Ord, Read, Show)

instance (Manifold m, Manifold n, Coordinate m ~ Coordinate n) =>
        Manifold (ManifoldProduct m n) where
    type Ok (ManifoldProduct m n) =
        (Ok m, Ok n, ManifoldOk (ManifoldProduct m n))
    type Dimension (ManifoldProduct m n) = Either (Dimension m) (Dimension n)
    type Point (ManifoldProduct m n) = (Point m, Point n)
    type Coordinate (ManifoldProduct m n) = Coordinate m
    volume (ManifoldProduct xs ys) = volume xs * volume ys
    bounds (ManifoldProduct xs _) (Left d) = bounds xs d
    bounds (ManifoldProduct _ ys) (Right d) = bounds ys d
    isValid (ManifoldProduct xs ys) (p, q) = isValid xs p && isValid ys q



-- | The sum of two manifolds
data ManifoldSum m n = ManifoldSum m n
                       deriving (Eq, Ord, Read, Show)

instance (Manifold m, Manifold n, Coordinate m ~ Coordinate n) =>
        Manifold (ManifoldSum m n) where
    type Ok (ManifoldSum m n) = (Ok m, Ok n, ManifoldOk (ManifoldSum m n))
    type Dimension (ManifoldSum m n) = Either (Dimension m) (Dimension n)
    type Point (ManifoldSum m n) = Either (Point m) (Point n)
    type Coordinate (ManifoldSum m n) = Coordinate m
    volume (ManifoldSum xs ys) = volume xs + volume ys
    bounds (ManifoldSum xs _) (Left d) = bounds xs d
    bounds (ManifoldSum _ ys) (Right d) = bounds ys d
    isValid (ManifoldSum xs _) (Left p) = isValid xs p
    isValid (ManifoldSum _ ys) (Right p) = isValid ys p



-- | One-dimensional interval
data Interval1 a = Interval1 { lo_ :: a
                             , hi_ :: a
                             }
    deriving (Eq, Ord, Read, Show)

instance RealFrac a => Manifold (Interval1 a) where
    type Dimension (Interval1 a) = ()
    type Point (Interval1 a) = a
    type Coordinate (Interval1 a) = a
    volume (Interval1 lo hi) = hi - lo
    bounds (Interval1 lo hi) () = (lo, hi)
    isValid (Interval1 lo hi) x = lo <= x && x <= hi
