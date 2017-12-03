{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module WaveToy2 ( State(..)
                , skeletonState
                , coordState
                , integralState
                , normState
                , initState
                , errorState
                , energyState
                , rhsState
                , rk2State
                ) where

import Control.Applicative
import Control.Comonad
import Data.VectorSpace

import Cell
import Chart
import Field
import SimpleVectors



-- | State vector for the whole simulation
data State b a = State { time :: b
                       , cells :: PiecewiseLinearField1D b a
                       }
                 deriving (Read, Show, Foldable, Functor)

instance Traversable (State b) where
    traverse f g = setcells g <$> traverse f (cells g)
        where setcells g c = g {cells = c}

instance Eq b => Applicative (State b) where
    pure _ = error "Cannot create State with unknown size"
    fg <*> g = g {cells = cells fg <*> cells g}

instance (Eq b, AdditiveGroup a) => AdditiveGroup (State b a) where
    zeroV = error "cannot construct null vector of unknown size"
    (^+^) = liftA2 (^+^)
    negateV = liftA negateV

instance (Eq b, VectorSpace a) => VectorSpace (State b a) where
    type Scalar (State b a) = Scalar a
    a *^ g = fmap (a *^) g

integralState :: (RealFrac b, VectorSpace a, Fractional (Scalar a)) =>
                 State b a -> a
integralState g = integral (cells g)

normState :: (RealFrac b, InnerSpace a, Floating (Scalar a)) =>
             State b a -> Scalar a
normState g = sqrt (cells g <.> cells g)

skeletonState :: Num a => (a, a) -> Int -> State a (V0 a)
skeletonState (lo, hi) np =
    State 0 (skeletonPiecewiseLinearField1D (Interval1 lo hi) np)

coordState :: RealFrac a => State a b -> State a a
coordState g = g {cells = newcells}
    where newcells = coordinatePiecewiseLinearField1D (chart_ (cells g)) np
          np = length (values (cells g))

initState :: RealFloat a => a -> State a b -> State a (Cell a)
initState t g = fmap init (coordState g) {time = t}
    where init x = initCell (t, x)

errorState :: (Eq a, RealFloat a) => State a (Cell a) -> State a (Cell a)
errorState g = error <$> coordState g <*> g
    where error x c = errorCell (time g, x) c

energyState :: Fractional a => State a (Cell a) -> State a a
energyState g = fmap energyCell g

rhsState :: (RealFrac b, VectorSpace a, RealFrac a, Fractional (Scalar a)) =>
            State b (Cell a) -> State b (Cell a)
rhsState g = g {cells = extend rhs (cells g)}
    where dx u = derivative () u
          bx u = boundaryNormal () u
          rhs u = rhsCell (bx u) (extract u) (dx u)

rk2State ::
    (VectorSpace (c a), Eq a, Fractional a, a ~ Scalar (c a)) =>
    a -> (State a (c a) -> State a (c a)) -> State a (c a) -> State a (c a)
rk2State dt rhs s0 =
    let r0 = rhs s0
        s1 = (s0 ^+^ (dt / 2) *^ r0) {time = time s0 + dt / 2}
        r1 = rhs s1
        s2 = (s0 ^+^ dt *^ r1) {time = time s0 + dt}
    in s2
