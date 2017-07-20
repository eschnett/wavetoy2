{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}

module MTorus where

import Control.Applicative
import Control.Comonad
import Data.Ratio
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.VectorSpace
import Prelude hiding ((!))

import Manifold

data MTorus a =
    VM Int
       (V.Vector a)
    deriving (Read, Show)

shift :: Int -> MTorus a -> MTorus a
shift offset (VM pos xs) = VM ((pos + offset) `mod` V.length xs) xs

instance Foldable MTorus where
    foldMap f (VM _ xs) = foldMap f xs

instance Functor MTorus where
    fmap f (VM pos xs) = VM pos (fmap f xs)

instance Applicative MTorus where
    pure _ = error "Cannot create Vector with unknown size"
    (VM _ fs) <*> (VM pos xs) =
        VM pos (V.generate (V.length xs) $ \i -> (fs ! i) (xs ! i))

instance Comonad MTorus where
    extract (VM pos xs) = xs ! pos
    extend f m@(VM pos xs) =
        VM pos $ V.generate (length xs) $ \i -> f (shift i m)

instance AdditiveGroup a => AdditiveGroup (MTorus a) where
    zeroV = pure zeroV
    xs ^+^ ys = (^+^) <$> xs <*> ys
    negateV xs = negateV <$> xs

instance VectorSpace a => VectorSpace (MTorus a) where
    type Scalar (MTorus a) = Scalar a
    alpha *^ xs = (alpha *^) <$> xs

instance MetricSpace MTorus where
    densitize xs = xs

instance Manifold MTorus where
    dimension _ = 1

instance DifferentiableManifold MTorus where
    derivative _ xs =
        fromRational (1 % 2) *^
        (extract (shift 1 xs) ^-^ extract (shift (-1) xs))
