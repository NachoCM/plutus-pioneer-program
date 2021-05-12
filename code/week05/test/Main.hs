-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty

import qualified Week05.Homework1Tests as Homework1Tests


main :: IO ()
main = defaultMain $ testGroup "plutus-pioneer-program tests"
  [ Homework1Tests.tests
  ]
