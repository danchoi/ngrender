module Ng.Tests where
import Test.HUnit 


runTests = runTestTT tests

test1 = TestCase (assertEqual "eq (1,2)," (1,2) (1,2))

tests = TestList [TestLabel "test1" test1]




