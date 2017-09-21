{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Cell ( Cell(..)
            , initCell
            , errorCell
            , energyCell
            , rhsCell
            ) where

import Control.Applicative
import Data.VectorSpace

default (Int)



-- | A cell holds the scalar wave equation state vector for a single grid point
data Cell a = Cell { u, rho, vx :: a }
              deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance Applicative Cell where
    pure x = Cell x x x
    Cell fu frho fvx <*> Cell xu xrho xvx = Cell (fu xu) (frho xrho) (fvx xvx)

instance AdditiveGroup a => AdditiveGroup (Cell a) where
    zeroV = Cell zeroV zeroV zeroV
    (^+^) = liftA2 (^+^)
    negateV = liftA negateV

instance VectorSpace a => VectorSpace (Cell a) where
    type Scalar (Cell a) = Scalar a
    a *^ x = fmap (a *^) x

instance InnerSpace a => InnerSpace (Cell a) where
    x <.> y = sumV ((<.>) <$> x <*> y)

sineCell :: Floating a => (a, a) -> Cell a
sineCell (t, x) =
    Cell { u = cos (2 * pi * t) * sin (2 * pi * x)
         , rho = -2 * pi * sin (2 * pi * t) * sin (2 * pi * x)
         , vx = 2 * pi * cos (2 * pi * t) * cos (2 * pi * x)
         }

initCell :: Floating a => (a, a) -> Cell a
initCell = sineCell

errorCell :: Floating a => (a, a) -> Cell a -> Cell a
errorCell (t, x) cell = liftA2 (-) cell (initCell (t, x))

energyCell :: Fractional a => Cell a -> a
energyCell c = 1/2 * (rho c ^ 2 + vx c ^ 2)

-- Dirichlet boundary conditions applied to the RHS
rhsCell :: (Num b, Ord b, Num a) => b -> Cell a -> Cell a -> Cell a
rhsCell bx c cx = mix bx
    where rhs = Cell (rho c) (vx cx) (rho cx)
          bclo = Cell (rho c) 0 (rho cx)
          bchi = Cell (rho c) 0 (rho cx)
          mix n | n < 0 = bclo
                | n > 0 = bchi
                | otherwise = rhs
