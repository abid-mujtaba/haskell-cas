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
            ,

            TestLabel "Rendering pre-ordered constants, symbols and sums being multiplied" $
                TestCase $ do

                    aE "test1" "x y" $ show (x * y)
                    aE "test2" "2 z" $ show (2 * z)
                    aE "test3" "-3 x" $ show (-3 * x)
                    aE "test4" "2 (x + y)" $ show (2 * (x + y))
                    aE "test5" "2 (x + y) z" $ show (2 * (x + y) * z)
            ,

            TestLabel "Rendering pre-ordered sum of products" $
                TestCase $ do

                    aE "test1" "(2 x + y)" $ show (2 * x + y)
                    aE "test2" "(-3 x + z)" $ show (-3 * x + z)
                    aE "test3" "(2 x y + 3 z)" $ show (2 * x * y + 3 * z)
            ,

            TestLabel "Rendering (pre-ordered) products of exponents" $
                TestCase $ do

                    aE "test1" "x^2" $ show (x^2)
                    aE "test2" "x^3" $ show (x^3)
                    aE "test3" "x^2 y" $ show (x^2 * y)
                    aE "test4" "x y^2" $ show (x * y^2)
                    aE "test5" "x (y + z)" $ show (x * (y + z))
                    aE "test6" "(x + y)^2" $ show ((x + y)^2)
            ,

            TestLabel "Rendering divided terms" $
                TestCase $ do

                    aE "test1" "1 / x" $ show (1 / x)
                    aE "test2" "x / y" $ show (x / y)
                    aE "test3" "1 / x^2" $ show (1 / x^2)
                    aE "test4" "x / (y z)" $ show (x / (y*z))
                    aE "test5" "x / (y + z)" $ show (x / (y + z))
                    aE "test6" "x / 2" $ show (x / 2)
                    aE "test7" "(x + y) / 2" $ show ((x + y) / 2)
        ]
