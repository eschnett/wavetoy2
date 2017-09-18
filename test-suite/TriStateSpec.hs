module TriStateSpec (spec) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck hiding (scale)

import TriState

import Data.Monoid



spec :: Spec
spec = parallel $ do
         it "obeys the rules" $
            property $
            mempty == Neither &&
            Neither <> Neither == Neither &&
            Neither <> This == This &&
            Neither <> That == That &&
            This <> Neither == This &&
            This <> This == This &&
            -- This <> That == error &&
            That <> Neither == That &&
            -- That <> This == error &&
            That <> That == That
