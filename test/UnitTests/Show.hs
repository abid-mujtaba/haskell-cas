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
            ,

            TestLabel "Rendering pre-ordered addition of symbols and constants" $
                TestCase $ do

                    aE "test1" "(x + y)" $ show (x + y)
                    aE "test2" "(y + z)" $ show (y + z)
                    aE "test3" "(x + z)" $ show (x + z)
                    aE "test4" "(x + 2)" $ show (x + 2)
                    aE "test5" "(y + 3)" $ show (y + 3)
            ,

            TestLabel "Rendering pre-ordered addition of more than two symbols and constants" $
                TestCase $ do

                    aE "test1" "(x + y + z)" $ show (x + y + z)
                    aE "test2" "(x + y + 3)" $ show (x + y + 3)
            ,

            TestLabel "Rendering subtraction of symbols and constants" $
                TestCase $ do

                    aE "test1" "(x - y)" $ show (x - y)
                    aE "test2" "(x - 3)" $ show (x - 3)
                    aE "test3" "(x + y - z)" $ show (x + y - z)
                    aE "test4" "(x - y + z)" $ show (x - y + z)
            ,

            TestLabel "Rendering addition and subtraction where the first symbol is negative" $
                TestCase $ do

                    aE "test1" "(-x + y)" $ show (-x + y)
                    aE "test2" "(-x - y)" $ show (-x - y)
        ]
