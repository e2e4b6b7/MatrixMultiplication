import qualified SmallMatrices
import qualified StrangeForms
import Test.Tasty.HUnit (Assertion)

main :: Assertion
main = do
  SmallMatrices.runTests
  StrangeForms.runTests
