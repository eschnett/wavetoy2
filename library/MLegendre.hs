{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}

module MLegendre where

import Control.Applicative
import Control.Comonad
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.VectorSpace
import Prelude hiding ((!))

import Manifold

data MLegendre a =
    VM Int
       (V.Vector a)
    deriving (Read, Show)

shift :: Int -> MLegendre a -> MLegendre a
shift offset (VM pos xs) = VM ((pos + offset) `mod` V.length xs) xs

instance Foldable MLegendre where
    foldMap f (VM _ xs) = foldMap f xs

instance Functor MLegendre where
    fmap f (VM pos xs) = VM pos (fmap f xs)

instance Applicative MLegendre where
    pure _ = error "Cannot create Vector with unknown size"
    (VM _ fs) <*> (VM pos xs) =
        VM pos (V.generate (V.length xs) $ \i -> (fs ! i) (xs ! i))

instance Comonad MLegendre where
    extract (VM pos xs) = xs ! pos
    extend f m@(VM pos xs) =
        VM pos $ V.generate (length xs) $ \i -> f (shift i m)

instance AdditiveGroup a => AdditiveGroup (MLegendre a) where
    zeroV = pure zeroV
    xs ^+^ ys = (^+^) <$> xs <*> ys
    negateV xs = negateV <$> xs

instance VectorSpace a => VectorSpace (MLegendre a) where
    type Scalar (MLegendre a) = Scalar a
    alpha *^ xs = (alpha *^) <$> xs

instance MetricSpace MLegendre where
    densitize (VM pos xs) = VM pos (V.imap (\i x -> coeff i *^ x) xs)
      where
        coeff i
            | i == 0 = 2
        coeff _
            | otherwise = 0

instance Manifold MLegendre where
    dimension _ = 1

instance DifferentiableManifold MLegendre where
    derivative _ (VM pos xs) =
        sumV [coeff i *^ xs ! i | i <- [pos - 1,pos - 3 .. 0]]
      where
        coeff :: Floating a => Int -> a
        coeff i = 2 / sqrt (2 / (2 * fromIntegral i + 1))
