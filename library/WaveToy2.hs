{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module WaveToy2
    ( L2Norm(..)
    , makeL2Norm
    , getL2Norm
    , Cell(..)
    , initCell
    , errorCell
    , energyCell
    , rhsCell
    , bcCell
    , Grid(..)
    , integralGrid
    , normGrid
    , skeletonGrid
    , coordGrid
    , initGrid
    , errorGrid
    , energyGrid
    , rhsGrid
    , bcGrid
    , rk2Grid
    ) where

import Control.Applicative
import Control.Comonad
import Control.Exception.Base
import Data.Foldable
import Data.Monoid
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.VectorSpace hiding (Sum(..))
import Prelude hiding ((!))

import Manifold

default (Int)

-- |An L2Norm holds the state for calculating an L2 norm
data L2Norm a =
    L2Norm a
           a
    deriving (Eq, Ord, Read, Show)

makeL2Norm :: Num a => a -> L2Norm a
makeL2Norm x = L2Norm 1 (x ^ 2)

getL2Norm :: Floating a => L2Norm a -> a
getL2Norm (L2Norm cnt ssq) = sqrt (ssq / cnt)

instance Num a => Monoid (L2Norm a) where
    mempty = L2Norm 0 0
    mappend (L2Norm cnt1 ssq1) (L2Norm cnt2 ssq2) =
        L2Norm (cnt1 + cnt2) (ssq1 + ssq2)

-- This is not really a group...
instance AdditiveGroup a => AdditiveGroup (L2Norm a) where
    zeroV = L2Norm zeroV zeroV
    L2Norm cx sx2 ^+^ L2Norm cy sy2 = L2Norm (cx ^+^ cy) (sx2 ^+^ sy2)
    -- negateV cannot be defined correctly
    negateV (L2Norm cx sx2) = L2Norm (negateV cx) sx2

instance VectorSpace a => VectorSpace (L2Norm a) where
    type Scalar (L2Norm a) = Scalar a
    alpha *^ L2Norm cx sx2 = L2Norm (alpha *^ cx) (alpha *^ sx2)

-- |A cell holds the scalar wave equation state vector for a single
-- grid point.
data Cell a = Cell
    { u, rho, vx :: a
    } deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance Applicative Cell where
    pure x = Cell x x x
    Cell fu frho fvx <*> Cell u rho vx = Cell (fu u) (frho rho) (fvx vx)

instance AdditiveGroup a => AdditiveGroup (Cell a) where
    zeroV = Cell zeroV zeroV zeroV
    (^+^) = liftA2 (^+^)
    negateV = liftA negateV

instance VectorSpace a => VectorSpace (Cell a) where
    type Scalar (Cell a) = Scalar a
    a *^ x = fmap (a *^) x

sineCell :: Floating a => (a, a) -> Cell a
sineCell (t, x) =
    Cell
    { u = cos (2 * pi * t) * sin (2 * pi * x)
    , rho = -2 * pi * sin (2 * pi * t) * sin (2 * pi * x)
    , vx = 2 * pi * cos (2 * pi * t) * cos (2 * pi * x)
    }

initCell :: Floating a => (a, a) -> Cell a
initCell = sineCell

errorCell :: Floating a => (a, a) -> Cell a -> Cell a
errorCell (t, x) cell = liftA2 (-) cell (initCell (t, x))

energyCell :: Fractional a => Cell a -> a
energyCell c = 1 / 2 * ((rho c) ^ 2 + (vx c) ^ 2)

rhsCell :: Cell a -> Cell a -> Cell a
rhsCell c cx = Cell (rho c) (vx cx) (rho cx)

-- Reflecting boundaries: u -> -u, rho -> -rho, vx -> vx
bcCell :: Num a => Cell a -> Cell a
bcCell c = Cell (-u c) (-rho c) (vx c)

-- |A grid holds the state vector for the whole simulation domain.
data Grid b a = Grid
    { time :: b
    , bnds :: (b, b)
    , origin :: Int
    , cells :: V.Vector a
    } deriving (Read, Show)

npGrid :: Grid b a -> Int
npGrid g = V.length (cells g)

dxGrid :: Fractional b => Grid b a -> b
dxGrid g = (xmax - xmin) / (fromIntegral np - 3)
  where
    (xmin, xmax) = bnds g
    np = npGrid g

instance Foldable (Grid b) where
    foldMap f g = foldMap f (cells g)

instance Functor (Grid b) where
    fmap f g = g {cells = fmap f (cells g)}

instance Applicative (Grid b) where
    pure x = error "Cannot create Grid with unknown size"
    fg <*> g = g {cells = V.generate (npGrid g) $ \i -> (fs ! i) (xs ! i)}
      where
        fs = cells fg
        xs = cells g

instance Comonad (Grid b) where
    extract g = cells g ! origin g
    extend f g = g {cells = V.generate (npGrid g) $ \i -> f (g {origin = i})}

instance AdditiveGroup a => AdditiveGroup (Grid b a) where
    zeroV = error "cannot construct null vector of unknown size"
    (^+^) = liftA2 (^+^)
    negateV = liftA negateV

instance VectorSpace a => VectorSpace (Grid b a) where
    type Scalar (Grid b a) = Scalar a
    a *^ g = fmap (a *^) g

instance MetricSpace (Grid b) where
    densitize g = g {cells = V.imap dens (cells g)}
      where
        dens i x
            | i == 0 || i == np - 1 = 0 *^ x
        dens i x
            | i == 1 || i == np - 2 = (1 / 2) *^ x
        dens i x
            | otherwise = x
        np = npGrid g

instance Manifold (Grid b) where
    dimension g = 1

instance RealFrac b => DifferentiableManifold (Grid b) where
    derivative d g = assert (d == 0) $ realToFrac (1 / dx) *^ deriv (origin g)
      where
        deriv i
            | i == 0 = xs ! 1 ^-^ xs ! 0
        deriv i
            | i == np - 1 = xs ! i ^-^ xs ! (i - 1)
        deriv i
            | otherwise = (1 / 2) *^ (xs ! (i + 1) ^-^ xs ! (i - 1))
        xs = cells g
        np = npGrid g
        dx = dxGrid g

integralGrid ::
       (VectorSpace a, Fractional (Scalar a), RealFrac b) => Grid b a -> a
integralGrid g = realToFrac (dxGrid g) *^ sumV (densitize g)

normGrid ::
       ( Foldable c
       , VectorSpace a
       , Fractional (Scalar a)
       , Floating a
       , RealFrac b
       )
    => Grid b (c a)
    -> a
normGrid g = getL2Norm $ integralGrid $ fmap (foldMap makeL2Norm) g

skeletonGrid :: Num a => (a, a) -> Int -> Grid a ()
skeletonGrid bnds np =
    assert (np > 3) $ Grid 0 bnds 0 $ V.generate np (const ())

coordGrid :: Fractional a => Grid a b -> Grid a a
coordGrid g = g {cells = V.generate (npGrid g) coords}
  where
    coords i = xmin + dx * (fromIntegral i - 1)
    (xmin, _) = bnds g
    dx = dxGrid g

initGrid :: Floating a => a -> Grid a b -> Grid a (Cell a)
initGrid t g = fmap init (coordGrid g) {time = t}
  where
    init x = initCell (t, x)

errorGrid :: Floating a => Grid a (Cell a) -> Grid a (Cell a)
errorGrid g = error <$> coordGrid g <*> g
  where
    error x c = errorCell (time g, x) c

energyGrid :: Fractional a => Grid a (Cell a) -> Grid a a
energyGrid g = fmap energyCell g

derivGrid ::
       (VectorSpace (c a), Fractional (Scalar (c a)), RealFrac b)
    => Grid b (c a)
    -> Grid b (c a)
derivGrid g = extend (derivative 0) g

rhsGrid ::
       (VectorSpace a, Fractional (Scalar a), RealFrac b)
    => Grid b (Cell a)
    -> Grid b (Cell a)
rhsGrid g = rhsCell <$> g <*> derivGrid g

bcGrid :: Num a => Grid b (Cell a) -> Grid b (Cell a)
bcGrid g = extend bc g
  where
    bc g = bc' (origin g)
    bc' i
        | i == 0 = bcCell (cs ! 2)
    bc' i
        | i == np - 1 = bcCell (cs ! (np - 3))
    bc' i
        | otherwise = cs ! i
    cs = cells g
    np = npGrid g

rk2Grid ::
       (Fractional a, VectorSpace (c a), a ~ Scalar (c a))
    => a
    -> (Grid a (c a) -> Grid a (c a))
    -> Grid a (c a)
    -> Grid a (c a)
rk2Grid dt rhs s0 =
    let r0 = rhs s0
        s1 = (s0 ^+^ (dt / 2) *^ r0) {time = time s0 + dt / 2}
        r1 = rhs s1
        s2 = (s0 ^+^ dt *^ r1) {time = time s0 + dt}
    in s2
