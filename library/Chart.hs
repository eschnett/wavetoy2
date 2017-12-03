{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Chart
    ( Chart(..)
    , ChartOk
    , ChartProduct(..)
    , chartProduct
    , ChartSum(..)
    , chartSum
    , Interval1(..)
    ) where

import Data.Kind
import Data.Void
import Test.QuickCheck



-- | A chart
class Chart m where
    type Ok m :: Constraint
    type Ok m = ChartOk m
    type Dimension m :: Type
    type Point m :: Type
    type Coordinate m :: Type
    empty :: Ok m => m -> Bool
    discrete :: Ok m => m -> Dimension m -> Bool
    volume :: Ok m => m -> Coordinate m
    bounds :: Ok m => m -> Dimension m -> (Coordinate m, Coordinate m)
    valid :: Ok m => m -> Point m -> Bool

type ChartOk m = (() :: Constraint) -- RealFrac (Coordinate m)



-- | The empty chart (with no points)
data EmptyChart a = EmptyChart
    deriving (Eq, Ord, Read, Show)

instance Arbitrary (EmptyChart a) where
    arbitrary = return EmptyChart

instance Chart (EmptyChart a) where
    type Ok (EmptyChart a) = (ChartOk (EmptyChart a), Num a)
    type Dimension (EmptyChart a) = ()
    type Point (EmptyChart a) = Void
    type Coordinate (EmptyChart a) = a -- or Void
    empty EmptyChart = True
    discrete EmptyChart () = True
    volume EmptyChart = 0
    bounds EmptyChart () = (1, 0)
    valid EmptyChart = absurd



-- | The unit chart (with one point)
data UnitChart a = UnitChart
    deriving (Eq, Ord, Read, Show)

instance Arbitrary (UnitChart a) where
    arbitrary = return UnitChart

instance Chart (UnitChart a) where
    type Ok (UnitChart a) = (ChartOk (UnitChart a), Num a)
    type Dimension (UnitChart a) = ()
    type Point (UnitChart a) = ()
    type Coordinate (UnitChart a) = a -- or ()
    empty UnitChart = False
    discrete UnitChart () = True
    volume UnitChart = 1
    bounds UnitChart () = (0, 0)
    valid UnitChart () = True



-- | The product of two charts
data ChartProduct m n = ChartProduct m n
    deriving (Eq, Ord, Read, Show)

-- chartProduct :: (Chart m, Chart n) => m -> n -> ChartProduct m n
chartProduct :: m -> n -> ChartProduct m n
chartProduct m n = ChartProduct m n

instance (Arbitrary m, Arbitrary n) => Arbitrary (ChartProduct m n) where
   arbitrary = do m <- arbitrary
                  n <- arbitrary
                  return $ ChartProduct m n
   shrink (ChartProduct m n) =
       [ChartProduct m' n' | (m', n') <- shrink (m, n)]

instance (Chart m, Chart n, Coordinate m ~ Coordinate n) =>
        Chart (ChartProduct m n) where
    type Ok (ChartProduct m n) =
        (Ok m, Ok n, ChartOk (ChartProduct m n), Num (Coordinate m))
    type Dimension (ChartProduct m n) = Either (Dimension m) (Dimension n)
    type Point (ChartProduct m n) = (Point m, Point n)
    type Coordinate (ChartProduct m n) = Coordinate m
    empty (ChartProduct xs ys) = empty xs || empty ys
    discrete (ChartProduct xs _) (Left d) = discrete xs d
    discrete (ChartProduct _ ys) (Right d) = discrete ys d
    volume (ChartProduct xs ys) = volume xs * volume ys
    bounds (ChartProduct xs _) (Left d) = bounds xs d
    bounds (ChartProduct _ ys) (Right d) = bounds ys d
    valid (ChartProduct xs ys) (p, q) = valid xs p && valid ys q



-- | The sum of two charts
data ChartSum m n = ChartSum m n
    deriving (Eq, Ord, Read, Show)

-- chartSum :: (Chart m, Chart n) => m -> n -> ChartSum m n
chartSum :: m -> n -> ChartSum m n
chartSum m n = ChartSum m n

instance (Arbitrary m, Arbitrary n) => Arbitrary (ChartSum m n) where
   arbitrary = do m <- arbitrary
                  n <- arbitrary
                  return $ ChartSum m n
   shrink (ChartSum m n) =
       [ChartSum m' n' | (m', n') <- shrink (m, n)]

instance (Chart m, Chart n, Coordinate m ~ Coordinate n) =>
        Chart (ChartSum m n) where
    type Ok (ChartSum m n) =
        (Ok m, Ok n, ChartOk (ChartSum m n), Num (Coordinate m))
    type Dimension (ChartSum m n) = Either (Dimension m) (Dimension n)
    type Point (ChartSum m n) = Either (Point m) (Point n)
    type Coordinate (ChartSum m n) = Coordinate m
    empty (ChartSum xs ys) = empty xs && empty ys
    discrete (ChartSum xs _) (Left d) = discrete xs d
    discrete (ChartSum _ ys) (Right d) = discrete ys d
    volume (ChartSum xs ys) = volume xs + volume ys
    bounds (ChartSum xs _) (Left d) = bounds xs d
    bounds (ChartSum _ ys) (Right d) = bounds ys d
    valid (ChartSum xs _) (Left p) = valid xs p
    valid (ChartSum _ ys) (Right p) = valid ys p



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
       | empty m = []
       --- | discrete m () = [iempty]
       --- | (lo, hi) == (0, 1) = [iempty, idiscrete]
       --- | lo == 0 && hi /= 1 = [iempty, idiscrete, inatural]
       --- | otherwise = [iempty, idiscrete, inatural, iextended]
       | discrete m () = [iempty]
       | (lo, hi) == (0, 1) = [idiscrete]
       | lo == 0 && hi /= 1 = [inatural]
       | otherwise = [iextended]
       where iempty = Interval1 1 0
             idiscrete = Interval1 0 0
             inatural = Interval1 0 1
             iextended = Interval1 0 (hi - lo)

instance Chart (Interval1 a) where
    type Ok (Interval1 a) = (ChartOk (Interval1 a), Num a, Ord a)
    type Dimension (Interval1 a) = ()
    type Point (Interval1 a) = a
    type Coordinate (Interval1 a) = a
    empty (Interval1 lo hi) = lo > hi
    discrete (Interval1 lo hi) () = lo >= hi
    volume (Interval1 lo hi) = max 0 (hi - lo)
    bounds (Interval1 lo hi) () = (lo, hi)
    valid (Interval1 lo hi) x = lo <= x && x <= hi
