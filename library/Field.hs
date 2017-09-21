{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Field ( Field(..)
             , FieldOk
             , FieldCompose(..)
             , fieldCompose
             , FieldProduct(..)
             , fieldProduct
             , PiecewiseLinearField1D(..)
             , generatePiecewiseLinearField1D
             , skeletonPiecewiseLinearField1D
             , coordinatePiecewiseLinearField1D
             , samplePiecewiseLinearField1D
             ) where

import Control.Applicative
import Control.Comonad
import Control.Exception (assert)
import Data.Kind
import Data.Monoid
import Data.VectorSpace
import Test.QuickCheck

import Manifold hiding (Ok)
import qualified Manifold
import SimpleVectors
import TriState



-- | A field, which is a mapping from a manifold to a vector space
-- Is there a connection to the Env comonad?
class Field f where
    type GetManifold f :: Type
    type Ok f v :: Constraint
    type Ok f v = FieldOk f v
    getManifold :: Ok f v => f v -> GetManifold f
    evaluate :: Ok f v => Point (GetManifold f) -> f v -> v
    integral :: Ok f v => f v -> v
    derivative :: Ok f v => Dimension (GetManifold f) -> f v -> v
    boundary :: Ok f v => Dimension (GetManifold f) -> f v -> v
    boundaryNormal ::
        Ok f v => Dimension (GetManifold f) -> f v -> Coordinate (GetManifold f)

type FieldOk f v =
    ( Foldable f
    , Comonad f
    , Manifold (GetManifold f)
    , Manifold.Ok (GetManifold f)
    , VectorSpace v
    , Fractional (Scalar v)
    )



-- | The product of two fields, with a domain consisting of two
-- manifolds glued togeter
data FieldProduct f g v = FieldProduct TriState (f v) (g v)
    deriving (Eq, Ord, Read, Show, Foldable, Functor)

-- fieldProduct :: (Field f, Field g) => f v -> g v -> FieldProduct f g v
fieldProduct :: f v -> g v -> FieldProduct f g v
fieldProduct f g = FieldProduct Neither f g

instance (Applicative f, Applicative g) => Applicative (FieldProduct f g) where
    pure x = FieldProduct Neither (pure x) (pure x)
    FieldProduct t f1 f2 <*> FieldProduct t' x1 x2 =
        FieldProduct (t <> t') (f1 <*> x1) (f2 <*> x2)

instance (Comonad f, Comonad g) => Comonad (FieldProduct f g) where
    extract (FieldProduct This x1 _) = extract x1
    extract (FieldProduct That _ x2) = extract x2
    extract _ = error "Cannot extract at Neither"
    -- This definition is wrong; x1-in-x1 and x2-in-x2 are not
    -- "duplicated", i.e. their comonadic foci do not vary
    duplicate (FieldProduct t x1 x2) =
        FieldProduct t (fmap (const (FieldProduct This x1 x2)) x1)
                         (fmap (const (FieldProduct That x1 x2)) x2)
    -- extend f (FieldProduct b x1 x2) = _

instance (Arbitrary (f v), Arbitrary (g v)) =>
        Arbitrary (FieldProduct f g v) where
    arbitrary = do f <- arbitrary
                   g <- arbitrary
                   return $ FieldProduct Neither f g
    shrink (FieldProduct _ f g) =
        [FieldProduct Neither f' g' | (f', g') <- shrink (f, g)]

instance (Field f, Field g) => Field (FieldProduct f g) where
    type GetManifold (FieldProduct f g) =
        ManifoldSum (GetManifold f) (GetManifold g)
    type Ok (FieldProduct f g) v =
        ( Ok f v
        , Ok g v
        , FieldOk (FieldProduct f g) v
        , Coordinate (GetManifold f) ~ Coordinate (GetManifold g)
        )
    getManifold (FieldProduct _ f g) =
        manifoldSum (getManifold f) (getManifold g)
    evaluate (Left p) (FieldProduct _ xs _) = evaluate p xs
    evaluate (Right p) (FieldProduct _ _ ys) = evaluate p ys
    integral (FieldProduct _ xs ys) = integral xs ^+^ integral ys
    derivative (Left d) (FieldProduct This xs _) = derivative d xs
    derivative (Right d) (FieldProduct That _ ys) = derivative d ys
    derivative _ _ = error "error"
    boundary (Left d) (FieldProduct This xs _) = boundary d xs
    boundary (Right d) (FieldProduct That _ ys) = boundary d ys
    boundary _ _ = error "error"
    boundaryNormal (Left d) (FieldProduct This xs _) = boundaryNormal d xs
    boundaryNormal (Right d) (FieldProduct That _ ys) = boundaryNormal d ys
    boundaryNormal _ _ = error "error"



-- | The composition of two fields, with a domain consisting of the
-- Cartesian product of two manifolds
-- Note: This is a monadic composition, except that we wrap it in a
-- new type.
newtype FieldCompose f g v = FieldCompose (f (g v))
    deriving (Eq, Ord, Read, Show, Foldable, Functor)

-- fieldCompose :: (Field f, Field g) => f (g v) -> FieldCompose f g v
fieldCompose :: f (g v) -> FieldCompose f g v
fieldCompose f = FieldCompose f

instance (Traversable f, Traversable g) => Traversable (FieldCompose f g) where
    traverse f (FieldCompose x) = FieldCompose <$> traverse (traverse f) x

instance (Applicative f, Applicative g) => Applicative (FieldCompose f g) where
    pure x = FieldCompose (pure (pure x))
    FieldCompose f <*> FieldCompose x = FieldCompose ((<*>) <$> f <*> x)

instance (Comonad f, Comonad g, Traversable f, Applicative g) =>
        Comonad (FieldCompose f g) where
    extract (FieldCompose x) = extract (extract x)
    duplicate (FieldCompose x) =
        fmap FieldCompose $ FieldCompose $
        fmap sequenceA $
        duplicate $ fmap duplicate $ x

instance Arbitrary (f (g v)) => Arbitrary (FieldCompose f g v) where
    arbitrary = do f <- arbitrary
                   return $ FieldCompose f
    shrink (FieldCompose f) = [FieldCompose f' | f' <- shrink f]

instance (Field f, Field g) => Field (FieldCompose f g) where
    type GetManifold (FieldCompose f g) =
        ManifoldProduct (GetManifold f) (GetManifold g)
    type Ok (FieldCompose f g) v =
        ( Ok g v
        , Ok f (g v)
        , FieldOk g v
        , FieldOk f (g v)
        , FieldOk (FieldCompose f g) v
        , Coordinate (GetManifold f) ~ Coordinate (GetManifold g)
        )
    getManifold (FieldCompose f) =
        manifoldProduct (getManifold f) (getManifold (extract f))
    evaluate (p, q) (FieldCompose x) = evaluate q (evaluate p x)
    integral (FieldCompose x) = integral (integral x)
    derivative (Left d) (FieldCompose x) = extract (derivative d x)
    derivative (Right d) (FieldCompose x) = derivative d (extract x)
    boundary (Left d) (FieldCompose x) = extract (boundary d x)
    boundary (Right d) (FieldCompose x) = boundary d (extract x)
    boundaryNormal (Left d) (FieldCompose x) = boundaryNormal d x
    boundaryNormal (Right d) (FieldCompose x) = boundaryNormal d (extract x)



-- | A simple field, living on a 1D manifold
data PiecewiseLinearField1D c v =
    PiecewiseLinearField1D { manifold_ :: Interval1 c
                           , position_ :: Int
                           , values :: [v]
                           }
    deriving (Eq, Ord, Read, Show)

-- Constraints:
-- [Comonad]: not (null values)
-- empty manifold: absurd

generatePiecewiseLinearField1D ::
    Interval1 c -> [v] -> PiecewiseLinearField1D c v
generatePiecewiseLinearField1D m vs = PiecewiseLinearField1D m 0 vs

skeletonPiecewiseLinearField1D ::
    Interval1 c -> Int -> PiecewiseLinearField1D c (V0 c)
skeletonPiecewiseLinearField1D m np =
    generatePiecewiseLinearField1D m (replicate np V0)

coordinatePiecewiseLinearField1D ::
    RealFrac c => Interval1 c -> Int -> PiecewiseLinearField1D c c
coordinatePiecewiseLinearField1D m np =
    assert (np >= 0) $
    generatePiecewiseLinearField1D m [xmin + fromIntegral i * dx |
                                      i <- [0..np-1]]
        where (xmin, xmax) = bounds m ()
              dx = if np <= 1 then 0 else (xmax - xmin) / fromIntegral (np - 1)

samplePiecewiseLinearField1D ::
    RealFrac c => Interval1 c -> Int -> (c -> v) -> PiecewiseLinearField1D c v
samplePiecewiseLinearField1D m np f =
    f <$> coordinatePiecewiseLinearField1D m np

instance Foldable (PiecewiseLinearField1D c) where
    foldMap f (PiecewiseLinearField1D _ _ xs) = foldMap f xs

instance Functor (PiecewiseLinearField1D c) where
    fmap f (PiecewiseLinearField1D m i xs) =
        PiecewiseLinearField1D m i (fmap f xs)

instance Traversable (PiecewiseLinearField1D c) where
    traverse f (PiecewiseLinearField1D m i xs) =
        PiecewiseLinearField1D m i <$> traverse f xs

instance Eq c => Applicative (PiecewiseLinearField1D c) where
    pure _ = error "undefined"
    PiecewiseLinearField1D m i fs <*> PiecewiseLinearField1D m' i' xs =
        assert (m == m' && i == i') $
        PiecewiseLinearField1D m i (getZipList $ ZipList fs <*> ZipList xs)

instance Comonad (PiecewiseLinearField1D c) where
    extract (PiecewiseLinearField1D _ i xs) = xs !! i
    duplicate (PiecewiseLinearField1D m i xs) =
        PiecewiseLinearField1D m i [PiecewiseLinearField1D m j xs |
                                    j <- [0..length xs-1]]
    -- extend f (PiecewiseLinearField1D m i xs) = _

instance (Eq c, AdditiveGroup v) =>
        AdditiveGroup (PiecewiseLinearField1D c v) where
    zeroV = pure zeroV
    (^+^) = liftA2 (^+^)
    negateV = liftA negateV

instance (Eq c, VectorSpace v) => VectorSpace (PiecewiseLinearField1D c v) where
    type Scalar (PiecewiseLinearField1D c v) = Scalar v
    a *^ g = fmap (a *^) g

-- TODO: use "integral" for this?
instance (Eq c, InnerSpace v, Fractional (Scalar v)) =>
        InnerSpace (PiecewiseLinearField1D c v) where
    x <.> y =  sumV (zipWith3 prod weights (values x) (values y)) / count
        where n = length (values x)
              w i = if i==0 || i==n-1 then 1/2 else 1
              weights = [w i | i <- [0..n-1]]
              prod w1 x1 y1 = w1 * (x1 <.> y1)
              count = realToFrac (n-1)

instance (Arbitrary c, Num c, Ord c, Arbitrary v) =>
        Arbitrary (PiecewiseLinearField1D c v) where
    arbitrary = do m <- arbitrary
                   xs <- arbitrary
                   return $ generatePiecewiseLinearField1D m xs
    shrink (PiecewiseLinearField1D m _ xs) =
        [PiecewiseLinearField1D m' 0 xs' | (m', xs') <- shrink (m, xs)]

instance Field (PiecewiseLinearField1D c) where
    type GetManifold (PiecewiseLinearField1D c) = Interval1 c
    type Ok (PiecewiseLinearField1D c) v =
        (FieldOk (PiecewiseLinearField1D c) v,
         RealFrac (Coordinate (GetManifold (PiecewiseLinearField1D c))))
    getManifold (PiecewiseLinearField1D m _ _) = m
    evaluate p (PiecewiseLinearField1D m _ xs) = assert (mfvalid m p) val
        where n = length xs
              (lo, hi) = bounds m ()
              x = fromIntegral (n-1) * (p - lo) / (hi - lo)
              i = max 0 $ min (n-2) $ floor x
              dx = x - fromIntegral i
              f0 = 1 - f1
              f1 = realToFrac dx
              val | n==0 = zeroV
                  | n==1 || mfdiscrete m () = xs !! 0
                  | otherwise = f0 *^ (xs !! i) ^+^ f1 *^ (xs !! (i+1))
    integral (PiecewiseLinearField1D m _ xs)
        | n == 0 = zeroV
        | n == 1 = realToFrac (volume m) *^ (xs !! 0)
        | otherwise = dV *^ sumV (zipWith (*^) ws xs)
        where n = length xs
              w i = if i==0 || i==n-1 then 1/2 else 1
              ws = [w i | i <- [0..n-1]]
              dV = realToFrac $ volume m / fromIntegral (n-1)
    derivative () (PiecewiseLinearField1D m i xs)
        | mfdiscrete m () || n <= 1 = zeroV
        | i==0 = dlo
        | i==n-1 = dhi
        | otherwise = dint
        where n = length xs
              dlo = (1 / realToFrac h) *^ ((xs !! 1) ^-^ (xs !! 0))
              dhi = (1 / realToFrac h) *^ ((xs !! (n-1)) ^-^ (xs !! (n-2)))
              dint = (1/2 / realToFrac h) *^ ((xs !! (i+1)) ^-^ (xs !! (i-1)))
              h = (mhi - mlo) / fromIntegral (n - 1)
              (mlo, mhi) = bounds m ()
    boundary () (PiecewiseLinearField1D m i xs)
        | mfdiscrete m () || n <= 1= zeroV
        | i==0 = blo
        | i==n-1 = bhi
        | otherwise = zeroV
        where n = length xs
              blo = (-2 / realToFrac h) *^ (xs !! 0)
              bhi = (2 / realToFrac h) *^ (xs !! (n-1))
              h = (mhi - mlo) / fromIntegral (n - 1)
              (mlo, mhi) = bounds m ()
    boundaryNormal () (PiecewiseLinearField1D m i xs)
        | mfdiscrete m () || n <= 1 = 0
        | i==0 = blo
        | i==n-1 = bhi
        | otherwise = 0
        where n = length xs
              blo = -1 / realToFrac h
              bhi = 1 / realToFrac h
              h = (mhi - mlo) / fromIntegral (n - 1)
              (mlo, mhi) = bounds m ()
