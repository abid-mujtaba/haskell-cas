-- Defines a list of the Unit Tests that test aspects of Multiplication

module UnitTests.Multiplication (tests)
    where

-- import Prelude hiding ((^))     -- This allows us to use the ^ operator defined in CAS without collision with Prelude.^
import Test.HUnit (Test(TestCase, TestLabel))

import CAS
import UnitTests.Base


tests = [
            TestLabel "Multiplying by identity (1)" $
                TestCase $ do

                    aE "test1" x (1 * x)
                    aE "test2" x (x * 1)
                    aE "test3" (x + y) ((x + y) * 1)
                    aE "test4" (x + y) (1 * (x + y))
                    aE "test5" (x * y) (x * y * 1)
                    aE "test6" (x * y) (x * 1 * y)
                    aE "test7" (x * y) (1 * x * y)

            -- TestLabel "Multiplicative Commutation" $
            --     TestCase $ do
            --
            --         let e1 = (x + 1)
            --         let e2 = (y + 2)
            --         let e3 = (z * (x + (2 * y)))
            --         let e4 = (((x * y) + 1) * (y + z))
            --
            --         aE "test1" (x * y) (y * x)
            --         aE "test2" (e1 * e2) (e2 * e1)
            --         aE "test3" (e3 * e4) (e4 * e3)
            -- ,
            --
            -- TestLabel "Division" $
            --     TestCase $ do
            --
            --         let w = Symbol "w"
            --         let q = w * x * y * z
            --
            --         aE "test1" 1 (x / x)
            --         aE "test2" x (x^2 / x)
            --         aE "test3" (1/x) (x/x^2)
            --         aE "test4" (x^2) (x^7 / x^5)
            --         aE "test5" (1/x^3) (x^5 / x^8)
            --         aE "test6" (x + y) ((x + y)^3 / (x + y)^2)
            --         aE "test7" (x * z) ((x * y * z) / y)
            --         aE "test8" (x * y * z) ((x * y^2 * z) / y)
            --         aE "test9" (w * x * z) (q / y)
            --         aE "test10" (1 / (w * x * z)) (y / q)
            -- ,
            --
            -- TestLabel "Multiplying Fractions" $
            --     TestCase $ do
            --
            --         let xi = 1/x
            --         let yi = 1/y
            --         let f1 = x/y
            --         let f2 = (y^3/x^2)
            --
            --         aE "test1" (2/x) (2 * xi)
            --         aE "test2" (y/x) (xi * y)
            --         aE "test3" (1/(x * y)) (xi * yi)
            --         aE "test4" (1/(x^2)) (xi * xi)
            --         aE "test5" 1 (x * xi)
            --         aE "test6" x (y * (x/y))
            --         aE "test7" xi (yi * (y/x))
            --         aE "test8" (y^2/x) (f1 * f2)
        ]
