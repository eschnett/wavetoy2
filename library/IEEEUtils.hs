module IEEEUtils where

import Numeric.IEEE



-- |Approximate floating-point comparison
(~~) :: (IEEE a, Num a) => a -> a -> Bool
(~~) = approxEq 1

infix 4 ~~

-- |Maximum of absolute values
absmaximum :: (Foldable t, Functor t, Num a, Ord a) => t a -> a
absmaximum = maximum . fmap abs



-- |Approximate floating-point comparison
approxEq' :: (IEEE a, Num a) => a -> a -> a -> Bool
approxEq' tol x y = abs (x - y) <= tol



-- |Approximate floating-point comparison
approxEq :: (IEEE a, Num a) => a -> a -> a -> Bool
approxEq scale x y = approxEq' (precision * epsilon * scale') x y
    where precision = 100
          scale' = absmaximum [abs x, abs y, scale]

-- |Approximate floating-point comparison
approxNe :: (IEEE a, Num a) => a -> a -> a -> Bool
approxNe scale x y = not (approxEq scale x y)

-- |Approximate floating-point comparison
approxLt :: (IEEE a, Num a) => a -> a -> a -> Bool
approxLt scale x y = x < y && not (approxEq scale x y)

-- |Approximate floating-point comparison
approxGt :: (IEEE a, Num a) => a -> a -> a -> Bool
approxGt scale x y = x > y && not (approxEq scale x y)

-- |Approximate floating-point comparison
approxLtEq :: (IEEE a, Num a) => a -> a -> a -> Bool
approxLtEq scale x y = x < y || approxEq scale x y

-- |Approximate floating-point comparison
approxGtEq :: (IEEE a, Num a) => a -> a -> a -> Bool
approxGtEq scale x y = x > y || approxEq scale x y
