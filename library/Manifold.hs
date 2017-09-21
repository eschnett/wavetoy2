{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Manifold
    ( Manifold(..)
    , ManifoldOk
    , ManifoldProduct(..)
    , manifoldProduct
    , ManifoldSum(..)
    , manifoldSum
    , Interval1(..)
    ) where

import Data.Kind
import Data.Void
import Test.QuickCheck



-- | A manifold
class Manifold m where
    type Ok m :: Constraint
    type Ok m = ManifoldOk m
    type Dimension m :: Type
    type Point m :: Type
    type Coordinate m :: Type
    mfempty :: Ok m => m -> Bool
    mfdiscrete :: Ok m => m -> Dimension m -> Bool
    volume :: Ok m => m -> Coordinate m
    bounds :: Ok m => m -> Dimension m -> (Coordinate m, Coordinate m)
    mfvalid :: Ok m => m -> Point m -> Bool

type ManifoldOk m = (() :: Constraint) -- RealFrac (Coordinate m)



-- | The empty manifold (with no points)
data EmptyManifold a = EmptyManifold
    deriving (Eq, Ord, Read, Show)

instance Arbitrary (EmptyManifold a) where
    arbitrary = return EmptyManifold

instance Manifold (EmptyManifold a) where
    type Ok (EmptyManifold a) = (ManifoldOk (EmptyManifold a), Num a)
    type Dimension (EmptyManifold a) = ()
    type Point (EmptyManifold a) = Void
    type Coordinate (EmptyManifold a) = a -- or Void
    mfempty EmptyManifold = True
    mfdiscrete EmptyManifold () = True
    volume EmptyManifold = 0
    bounds EmptyManifold () = (1, 0)
    mfvalid EmptyManifold = absurd



-- | The unit manifold (with one point)
data UnitManifold a = UnitManifold
    deriving (Eq, Ord, Read, Show)

instance Arbitrary (UnitManifold a) where
    arbitrary = return UnitManifold

instance Manifold (UnitManifold a) where
    type Ok (UnitManifold a) = (ManifoldOk (UnitManifold a), Num a)
    type Dimension (UnitManifold a) = ()
    type Point (UnitManifold a) = ()
    type Coordinate (UnitManifold a) = a -- or ()
    mfempty UnitManifold = False
    mfdiscrete UnitManifold () = True
    volume UnitManifold = 1
    bounds UnitManifold () = (0, 0)
    mfvalid UnitManifold () = True



-- | The product of two manifolds
data ManifoldProduct m n = ManifoldProduct m n
    deriving (Eq, Ord, Read, Show)

-- manifoldProduct :: (Manifold m, Manifold n) => m -> n -> ManifoldProduct m n
manifoldProduct :: m -> n -> ManifoldProduct m n
manifoldProduct m n = ManifoldProduct m n

instance (Arbitrary m, Arbitrary n) => Arbitrary (ManifoldProduct m n) where
   arbitrary = do m <- arbitrary
                  n <- arbitrary
                  return $ ManifoldProduct m n
   shrink (ManifoldProduct m n) =
       [ManifoldProduct m' n' | (m', n') <- shrink (m, n)]

instance (Manifold m, Manifold n, Coordinate m ~ Coordinate n) =>
        Manifold (ManifoldProduct m n) where
    type Ok (ManifoldProduct m n) =
        (Ok m, Ok n, ManifoldOk (ManifoldProduct m n), Num (Coordinate m))
    type Dimension (ManifoldProduct m n) = Either (Dimension m) (Dimension n)
    type Point (ManifoldProduct m n) = (Point m, Point n)
    type Coordinate (ManifoldProduct m n) = Coordinate m
    mfempty (ManifoldProduct xs ys) = mfempty xs || mfempty ys
    mfdiscrete (ManifoldProduct xs _) (Left d) = mfdiscrete xs d
    mfdiscrete (ManifoldProduct _ ys) (Right d) = mfdiscrete ys d
    volume (ManifoldProduct xs ys) = volume xs * volume ys
    bounds (ManifoldProduct xs _) (Left d) = bounds xs d
    bounds (ManifoldProduct _ ys) (Right d) = bounds ys d
    mfvalid (ManifoldProduct xs ys) (p, q) = mfvalid xs p && mfvalid ys q



-- | The sum of two manifolds
data ManifoldSum m n = ManifoldSum m n
    deriving (Eq, Ord, Read, Show)

-- manifoldSum :: (Manifold m, Manifold n) => m -> n -> ManifoldSum m n
manifoldSum :: m -> n -> ManifoldSum m n
manifoldSum m n = ManifoldSum m n

instance (Arbitrary m, Arbitrary n) => Arbitrary (ManifoldSum m n) where
   arbitrary = do m <- arbitrary
                  n <- arbitrary
                  return $ ManifoldSum m n
   shrink (ManifoldSum m n) =
       [ManifoldSum m' n' | (m', n') <- shrink (m, n)]

instance (Manifold m, Manifold n, Coordinate m ~ Coordinate n) =>
        Manifold (ManifoldSum m n) where
    type Ok (ManifoldSum m n) =
        (Ok m, Ok n, ManifoldOk (ManifoldSum m n), Num (Coordinate m))
    type Dimension (ManifoldSum m n) = Either (Dimension m) (Dimension n)
    type Point (ManifoldSum m n) = Either (Point m) (Point n)
    type Coordinate (ManifoldSum m n) = Coordinate m
    mfempty (ManifoldSum xs ys) = mfempty xs && mfempty ys
    mfdiscrete (ManifoldSum xs _) (Left d) = mfdiscrete xs d
    mfdiscrete (ManifoldSum _ ys) (Right d) = mfdiscrete ys d
    volume (ManifoldSum xs ys) = volume xs + volume ys
    bounds (ManifoldSum xs _) (Left d) = bounds xs d
    bounds (ManifoldSum _ ys) (Right d) = bounds ys d
    mfvalid (ManifoldSum xs _) (Left p) = mfvalid xs p
    mfvalid (ManifoldSum _ ys) (Right p) = mfvalid ys p



-- | One-dimensional interval
data Interval1 a = Interval1 { lo_, hi_ :: a }
    deriving (Eq, Ord, Read, Show)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Interval1 a) where
   arbitrary = do lo' <- arbitrary
                  hi' <- arbitrary
                  let lo = min lo' hi'
                  let hi = max lo' hi'
                  frequency [(1, return $ Interval1 hi lo),
                             (1, return $ Interval1 lo lo),
                             (8, return $ Interval1 lo hi)]
   shrink m@(Interval1 lo hi)
       | mfempty m = []
       --- | mfdiscrete m () = [iempty]
       --- | (lo, hi) == (0, 1) = [iempty, idiscrete]
       --- | lo == 0 && hi /= 1 = [iempty, idiscrete, inatural]
       --- | otherwise = [iempty, idiscrete, inatural, iextended]
       | mfdiscrete m () = [iempty]
       | (lo, hi) == (0, 1) = [idiscrete]
       | lo == 0 && hi /= 1 = [inatural]
       | otherwise = [iextended]
       where iempty = Interval1 1 0
             idiscrete = Interval1 0 0
             inatural = Interval1 0 1
             iextended = Interval1 0 (hi - lo)

instance Manifold (Interval1 a) where
    type Ok (Interval1 a) = (ManifoldOk (Interval1 a), Num a, Ord a)
    type Dimension (Interval1 a) = ()
    type Point (Interval1 a) = a
    type Coordinate (Interval1 a) = a
    mfempty (Interval1 lo hi) = lo > hi
    mfdiscrete (Interval1 lo hi) () = lo >= hi
    volume (Interval1 lo hi) = max 0 (hi - lo)
    bounds (Interval1 lo hi) () = (lo, hi)
    mfvalid (Interval1 lo hi) x = lo <= x && x <= hi
