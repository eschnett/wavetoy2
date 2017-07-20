{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Manifold
    ( MetricSpace(..)
    , integral
    , Manifold(..)
    , DifferentiableManifold(..)
    ) where

import Control.Comonad
import Data.VectorSpace

class Functor m =>
      MetricSpace m where
    densitize :: (VectorSpace a, Fractional (Scalar a)) => m a -> m a
    volumeForm :: (Num b, VectorSpace b, Fractional (Scalar b)) => m a -> m b
    volumeForm m = densitize (1 <$ m)
    -- Expected:
    --     instance (MetricSpace m, Applicative m, AdditiveGroup a) =>
    --         AdditiveGroup (m a)
    --     instance (MetricSpace m, Functor m, VectorSpace a) =>
    --         VectorSpace (m a)

integral ::
       (MetricSpace m, Foldable m, VectorSpace a, Fractional (Scalar a))
    => m a
    -> a
integral m = getSum $ foldMap Sum (densitize m)

class (MetricSpace m, Comonad m) =>
      Manifold m where
    dimension :: m a -> Int
    -- TODO:
    -- unityPartition :: RealFrac a => m a
    -- glue :: Int -> (m a -> m a, m a -> m a) -> m a -> m a -> m a

class Manifold m =>
      DifferentiableManifold m where
    derivative :: (VectorSpace a, Fractional (Scalar a)) => Int -> m a -> a
