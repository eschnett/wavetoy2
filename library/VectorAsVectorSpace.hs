{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module VectorAsVectorSpace where

import qualified Data.VectorSpace as V

-- import qualified Linear.Vector as L

-- instance (L.Additive f, V.AdditiveGroup a) => V.AdditiveGroup (f a) where
--     zeroV = L.zero
--     negateV = fmap negateV
--     (^+^) = (L.^+^)

-- instance (L.Additive f, V.VectorSpace a) => V.VectorSpace (f a) where
--     type Scalar (f a) = V.Scalar a
--     (*^) = (L.*^)
