-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

-- import Test.Tasty.QuickCheck

-- import Data.Functor.Identity
-- import Data.Vector ((!))
-- import qualified Data.Vector as V
-- import Numeric.IEEE
-- import Prelude hiding ((!))

import qualified CellSpec
import qualified FieldSpec
import qualified ManifoldSpec
import qualified NormSpec
import qualified TriStateSpec
import qualified WaveToy2Spec

-- default (Int)



main :: IO ()
main = do
    test <- testSpec "WaveToy2" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec =
    do describe "Norm" NormSpec.spec
       describe "TriState" TriStateSpec.spec
       describe "Manifold" ManifoldSpec.spec
       describe "Field" FieldSpec.spec
       describe "Cell" CellSpec.spec
       -- describe "WaveToy2" WaveToy2Spec.spec
