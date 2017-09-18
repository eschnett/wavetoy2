module TriState (TriState(..)) where

import Test.QuickCheck



-- | Tri-state logic, equivalent to Maybe Bool with Nothing as neutral value
data TriState = Neither | This | That
    deriving (Eq, Ord, Read, Show)

instance Monoid TriState where
    mempty = Neither
    mappend Neither x = x
    mappend x Neither = x
    mappend x y | x == y = x
                | otherwise = error "Cannot combine This and That"

instance Arbitrary TriState where
    arbitrary = frequency [(1, return Neither),
                           (1, return This),
                           (1, return That)]
