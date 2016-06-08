-- Defines a list of tests that test the "show" aspects of expressions
-- that is, how they are rendered in to strings by the "show" function

module UnitTests.Show
    where

import Test.HUnit (Test(TestCase, TestLabel))

import CAS
import UnitTests.Base


tests = [
            TestLabel "Rendering plain symbols" $
                TestCase $ do

                    aE "test1" "x" $ show x
                    aE "test2" "y" $ show y
                    aE "test3" "z" $ show z
        ]
