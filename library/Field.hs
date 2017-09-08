{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Field ( Field(..)
             , FieldOk
             , FieldCompose(..)
             , FieldProduct(..)
             , PiecewiseLinearField1D(..)
             , generatePiecewiseLinearField1D
             , skeletonPiecewiseLinearField1D
             , coordinatePiecewiseLinearField1D
             ) where

import Control.Applicative
import Control.Comonad
import Control.Exception (assert)
import Data.Kind
import Data.VectorSpace

import Manifold hiding (Ok)
import qualified Manifold as M

    

-- | A field, which is a mapping from a manifold to a vector space
-- Is there a connection to the Env comonad?
class Field f where
    type GetManifold f :: Type
    type Ok f v :: Constraint
    type Ok f v = FieldOk f v
    evaluate :: Ok f v => Point (GetManifold f) -> f v -> v
    integral :: Ok f v => f v -> v
    -- interior :: Ok f v => f v -> Scalar v
    -- boundary :: Ok f v => Dimension (GetManifold f) -> f v -> Scalar v
    derivative :: Ok f v => Dimension (GetManifold f) -> f v -> v
    boundaryNormal ::
        Ok f v =>
        Dimension (GetManifold f) -> f v -> Coordinate (GetManifold f)

type FieldOk f v = ( Foldable f
                   , Comonad f
                   , Manifold (GetManifold f)
                   , M.Ok (GetManifold f)
                   , VectorSpace v
                   , Fractional (Scalar v)
                   )



-- | The product of two fields, with a domain consisting of two
-- manifolds glued togeter
data FieldProduct f g v = FieldProduct Bool (f v) (g v)
                          deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance (Applicative f, Applicative g) => Applicative (FieldProduct f g) where
    pure x = FieldProduct False (pure x) (pure x)
    FieldProduct b f1 f2 <*> FieldProduct b' x1 x2 =
        assert (b==b') $
        FieldProduct b (f1 <*> x1) (f2 <*> x2)

instance (Comonad f, Comonad g) => Comonad (FieldProduct f g) where
    extract (FieldProduct b x1 x2) = if not b then extract x1 else extract x2
    duplicate (FieldProduct b x1 x2) =
        FieldProduct b (fmap (const (FieldProduct False x1 x2)) x1)
                         (fmap (const (FieldProduct True x1 x2)) x2)
    -- extend f (FieldProduct b x1 x2) = _

instance (Field f, Field g) => Field (FieldProduct f g) where
    type GetManifold (FieldProduct f g) =
        ManifoldSum (GetManifold f) (GetManifold g)
    type Ok (FieldProduct f g) v =
        ( Ok f v
        , Ok g v
        , FieldOk (FieldProduct f g) v
        , Coordinate (GetManifold f) ~ Coordinate (GetManifold g)
        )
    evaluate (Left p) (FieldProduct _ xs _) = evaluate p xs
    evaluate (Right p) (FieldProduct _ _ ys) = evaluate p ys
    integral (FieldProduct _ xs ys) = integral xs ^+^ integral ys
    -- interior (FieldProduct False xs _) = interior xs
    -- interior (FieldProduct True _ ys) = interior ys
    -- boundary (Left d) (FieldProduct False xs _) = boundary d xs
    -- boundary (Right d) (FieldProduct True _ ys) = boundary d ys
    derivative (Left d) (FieldProduct False xs _) = derivative d xs
    derivative (Right d) (FieldProduct True _ ys) = derivative d ys
    derivative _ _ = error "error"
    boundaryNormal (Left d) (FieldProduct False xs _) = boundaryNormal d xs
    boundaryNormal (Right d) (FieldProduct True _ ys) = boundaryNormal d ys
    boundaryNormal _ _ = error "error"



-- | The composition of two fields, with a domain consisting of the
-- Cartesian product of two manifolds
data FieldCompose f g v = FieldCompose (f (g v))
                          deriving (Eq, Ord, Read, Show, Foldable, Functor)
    
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
    evaluate (p, q) (FieldCompose x) = evaluate q (evaluate p x)
    integral (FieldCompose x) = integral (integral x)
    -- interior (FieldCompose x) = extract (interior x)
    -- boundary (Left d) (FieldCompose x) = extract (boundary d x)
    -- boundary (Right d) (FieldCompose x) = boundary d (extract x)
    derivative (Left d) (FieldCompose x) = extract (derivative d x)
    derivative (Right d) (FieldCompose x) = derivative d (extract x)
    boundaryNormal (Left d) (FieldCompose x) = boundaryNormal d x
    boundaryNormal (Right d) (FieldCompose x) = boundaryNormal d (extract x)



-- | A simple field, living on a 1D manifold
data PiecewiseLinearField1D c v =
    PiecewiseLinearField1D { manifold_ :: Interval1 c
                           , position_ :: Int
                           , values :: [v]
                           }
    deriving (Eq, Ord, Read, Show)

generatePiecewiseLinearField1D ::
    Interval1 c -> [v] -> PiecewiseLinearField1D c v
generatePiecewiseLinearField1D m vs = PiecewiseLinearField1D m 0 vs

skeletonPiecewiseLinearField1D ::
    Interval1 c -> Int -> PiecewiseLinearField1D c ()
skeletonPiecewiseLinearField1D m np =
    generatePiecewiseLinearField1D m (replicate np ())

coordinatePiecewiseLinearField1D ::
    RealFrac c => Interval1 c -> Int -> PiecewiseLinearField1D c c
coordinatePiecewiseLinearField1D m np =
    assert (np >= 0) $
    generatePiecewiseLinearField1D m [xmin + fromIntegral i * dx |
                                      i <- [0..np-1]]
        where (xmin, xmax) = bounds m ()
              dx = if np <= 1 then 0 else (xmax - xmin) / fromIntegral (np - 1)

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

instance (Eq c, InnerSpace v) => InnerSpace (PiecewiseLinearField1D c v) where
    x <.> y = sumV $ zipWith (<.>) (values x) (values y)

instance Field (PiecewiseLinearField1D c) where
    type GetManifold (PiecewiseLinearField1D c) = Interval1 c
    evaluate p (PiecewiseLinearField1D m _ xs) = val
        where n = length xs
              (lo, hi) = bounds m ()
              i = floor $ fromIntegral (n-1) * (p - lo) / (hi - lo)
              p0 = lo + fromIntegral i / fromIntegral (n-1) * (hi - lo)
              f0 = 1 - f1
              f1 = p - p0
              val = realToFrac f0 *^ xs !! i ^+^ realToFrac f1 *^ xs !! (i+1)
    integral (PiecewiseLinearField1D m _ xs) =
        if length xs < 2
        then zeroV
        else realToFrac dV *^ sumV (zipWith (*^) ws xs)
        where n = length xs
              w i = if i==0 || i==n-1 then 1/2 else 1
              ws = [w i | i <- [0..n-1]]
              dV = volume m / fromIntegral (n-1)
    -- interior f = 1 - boundary () f
    -- boundary () (PiecewiseLinearField1D _ i xs) =
    --     if n<=1 then 1
    --     else if i==0 || i==n-1 then 1 else 0
    --     where n = length xs
    derivative () (PiecewiseLinearField1D m i xs) =
        if n<=1 then zeroV
        else if i==0 then dlo
        else if i==n-1 then dhi
        else dint
        where n = length xs
              dlo = (1 / realToFrac h) *^ (xs !! 1 ^-^ xs !! 0)
              dhi = (1 / realToFrac h) *^ (xs !! (n-1) ^-^ xs !! (n-2))
              dint = (1 / (2 * realToFrac h)) *^ (xs !! (i+1) ^-^ xs !! (i-1))
              h = (mhi - mlo) / fromIntegral (n - 1)
              (mlo, mhi) = bounds m ()
    boundaryNormal () (PiecewiseLinearField1D m i xs) =
        if n<=1 then 0
        else if i==0 then blo
        else if i==n-1 then bhi
        else 0
        where n = length xs
              blo = -1 / realToFrac h
              bhi = 1 / realToFrac h
              h = (mhi - mlo) / fromIntegral (n - 1)
              (mlo, mhi) = bounds m ()
