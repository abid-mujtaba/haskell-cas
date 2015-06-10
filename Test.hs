--  Copyright 2015 Abid Hasan Mujtaba
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--
--
-- This module provides a test suite for the CAS module.
--
-- To run the tests inside GHCi load the module and then use the 'tests' object as follows:
--
-- > :l TestCAS
-- > runTestTT tests
--
-- Test tests can also be run by compiling the module and executing it. This task is automated in the Makefile so all
-- you need to do is run:
--
-- make test
--
--
-- Source for HUnit usage: https://wiki.haskell.org/HUnit_1.0_User's_Guide
-- Source for QuickCheck usage: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html



module Test
    (
        main,               -- We must export the main functions to create the relevant executables
        main_unit,
        main_quick,
        tests,
    )
    where


import Control.Applicative
import Debug.Trace(trace,traceShow)

import Prelude hiding ((^))     -- This allows us to use the ^ operator defined in CAS without collision with Prelude.^

import Test.HUnit
import Test.QuickCheck

import CAS
import Vars

main = do                     -- In the main function we simply run the tests. So running the executable (Test) will cause the tests to be executed
          main_unit
          main_quick


main_unit = do                -- This IO Action runs only the unit tests
                runTestTT tests

main_quick = do               -- This IO Action runs only the property checks
                quickTests



-- We add a label to the TestCase by using the TestLabel constructor.
-- The assertions are grouped together as a single TestCase
-- Since the assertions are IO () we use the 'do' keyword to group together a sequence of them
-- The first assertion that fails causes the entire TestCase to fail and the subsequent assertions are not tested
-- Each assertEqual call takes the format: aE <failure message> <expected value> <actual/tested value>

tests = TestList [                                              -- We create a list of TestCases

            TestLabel "Adding similar products" $
                TestCase $ do

                    let e = x + y

                    aE "test1" (2 * e) (e + e)
                    aE "test2" (5 * e) ((2 * e) + (3 * e))
                    aE "test3" (3 * e) ((-2 * e) + (5 * e))
                    aE "test4" (-7 * e) ((-3 * e) + (4 * (-e)))
            ,

            TestLabel "Subtracting equal expressions" $
                TestCase $ do

                    let e1 = -1 + x
--                    let e2 = 2 * (-x) / y
                    let e3 = -y + (2 * x * y)
                    let e4 = x + y
                    let e5 = -1 + e4
                    let e6 = e5 + z

                    aE "test2" 0 (x - x)
                    aE "test3" 0 (e1 - e1)
--                    aE "test4" 0 (e2 - e2)
                    aE "test5" 0 (e3 - e3)
                    aE "test6" 0 (e4 - e4)
                    aE "test7" 0 (e5 - e5)
                    aE "test8" 0 (e6 - e6)
            ,

            TestLabel "Order of Product Elements" $
                TestCase $ do

                    aE "test1" "(x * y)" $ show (x * y)
                    aE "test2" "(2 * x)" $ show (x * 2)
                    aE "test3" "(2 * x^2 * y)" $ show (2 * y * x^2)
                    aE "test4" "-(2 * x * y^2)" $ show (x * (-2) * y^2)
                    aE "test5" "(x * y * (y + z))" $ show (x * y * (y + z))
                    aE "test6" "(x * z^2 * (x + y))" $ show ((x + y) * z^2 * x)
                    aE "test7" "(x * z^2 * (x + y) * (y + z))" $ show ((y + z) * z^2 * (x + y) * x)
                    aE "test8" "(x * y^2 * (y + z)^3)" $ show ((y + z)^2 * y^2 * (y + z) * x)
                    aE "test9" "((y + z) * ((x * y) + 1))" $ show ((x * y + 1) * (y + z))
            ,

            TestLabel "Graded Reversed Lexical Order" $
                TestCase $ do

                    aE "test1"  EQ $ compare x x
                    aE "test2"  LT $ compare 2 x
                    aE "test3"  GT $ compare x y
                    aE "test4"  GT $ compare (x^2) x
                    aE "test5"  LT $ compare x (y^2)
                    aE "test6"  GT $ compare x (2 * y)
                    aE "test7"  LT $ compare (2 * y) (3 * x)
                    aE "test8"  EQ $ compare (2 * x) (2 * x)
                    aE "test9"  GT $ compare (x * y) (y * z)
                    aE "test10" LT $ compare (x * y) (x^2)
                    aE "test11" GT $ compare (x * y^2 * z) (x * y * z^2)
                    aE "test12" GT $ compare (x * y * z) (x * z^2)
                    aE "test13" EQ $ compare (x * y * z) (x * y * z)
                    aE "test14" EQ $ compare (x * y^2 * z) (x * y^2 * z)
                    aE "test15" LT $ compare x (2 * x)
                    aE "test16" GT $ compare (3 * x) x
                    aE "test17" LT $ compare x (-2 * x)             -- Negative constants shouldn't impact the order of terms
            ,

            TestLabel "Order of Added Elements" $
                TestCase $ do

                    aE "test1" "(x + 2)" $ show (x + 2)
                    aE "test2" "(x + y)" $ show (x + y)
                    aE "test3" "(x^2 + y)" $ show (x^2 + y)
                    aE "test4" "(y^2 + x)" $ show (x + y^2)
                    aE "test5" "(y + (2 * z) + 1)" $ show $ y + (1 + 2 * z)
                    aE "test6" "(x - y)" $ show (x - y)
                    aE "test7" "(x - y)" $ show (-y + x)
                    aE "test8" "(x/y + 1)" $ show (1 + x/y)
                    aE "test9" "(x + x/y)" $ show (x + x/y)
                    aE "test10" "(1/x + 1/y)" $ show (1/x + 1/y)
                    aE "test11" "(x/y + y/z)" $ show (x/y + y/z)
                    aE "test12" "(x/y + z/y)" $ show (x/y + z/y)
                    aE "test13" "(x^2/y + z^2/x)" $ show (x^2/y + z^2/x)
            ,

            TestLabel "Comparing expressions" $
                TestCase $ do

                    aE "test1" EQ $ compare x x
                    aE "test2" GT $ compare x y
                    aE "test3" LT $ compare y x
                    aE "test4" EQ $ compare (x + y) (y + x)
                    aE "test5" GT $ compare (x + y) (y + z)
                    aE "test6" GT $ compare (x - z - 2) (1 - y)
                    aE "test7" GT $ compare (x^2 + y^2) (z^2)
                    aE "test8" GT $ compare y (2*z)
                    aE "test9" LT $ compare z (3*y)
                    aE "test10" GT $ compare (x + 1) (y + 2)
                    aE "test11" GT $ compare x (-x)
                    aE "test12" GT $ compare (z + 1) (-z + 1)
                    aE "test13" GT $ compare (z^2) ((z + 1)^2)
            ,

            TestLabel "Additive Commutation" $
                TestCase $ do

                    let e2 = (1 + 2 * y)
                    let e3 = (2 + y)
                    let e4 = (1 + 2 * z)

                    let e5 = 9 * y * z^2
                    let e6 = z * (z + 1)^2

                    aE "test1" (x + y) (y + x)
                    aE "test2" (x + e2) (e2 + x)
                    aE "test3" (e3 + e4) (e4 + e3)
                    aE "test4" (e5 + e6) (e6 + e5)
            ,

            TestLabel "Multiplicative Commutation" $
                TestCase $ do

                    let e1 = (x + 1)
                    let e2 = (y + 2)
                    let e3 = (z * (x + (2 * y)))
                    let e4 = (((x * y) + 1) * (y + z))

                    aE "test1" (x * y) (y * x)
                    aE "test2" (e1 * e2) (e2 * e1)
                    aE "test3" (e3 * e4) (e4 * e3)
            ,

            TestLabel "Division" $
                TestCase $ do

                    let w = Symbol "w"
                    let q = w * x * y * z

                    aE "test1" 1 (x / x)
                    aE "test2" x (x^2 / x)
                    aE "test3" (1/x) (x/x^2)
                    aE "test4" (x^2) (x^7 / x^5)
                    aE "test5" (1/x^3) (x^5 / x^8)
                    aE "test6" (x + y) ((x + y)^3 / (x + y)^2)
                    aE "test7" (x * z) ((x * y * z) / y)
                    aE "test8" (x * y * z) ((x * y^2 * z) / y)
                    aE "test9" (w * x * z) (q / y)
                    aE "test10" (1 / (w * x * z)) (y / q)
            ,

            TestLabel "Multiplying Fractions" $
                TestCase $ do

                    let xi = 1/x
                    let yi = 1/y

                    aE "test1" (2/x) (2 * xi)
                    aE "test2" (y/x) (xi * y)
                    aE "test3" (1/(x * y)) (xi * yi)
                    aE "test4" (1/(x^2)) (xi * xi)
                    aE "test5" 1 (x * xi)
                    aE "test6" x (y * (x/y))
                    aE "test7" xi (yi * (y/x))
        ]



-- Define shorthand utility functions for assertions

aE :: (Eq a, Show a) => String -> a -> a -> Assertion
aE = assertEqual

aB :: String -> Bool -> Assertion
aB = assertBool


-- We define a composite IO action consisting of all quickCheck property tests defined in the module
-- If one wants a look at the generated expressions in any quickCheck simply replace the call with 'verboseCheck'. This is a good debugging strategy.
-- We use 'counterexample' to attach a label to each test which will be printed if the test fails. This will let us know at a glance what went wrong.

quickTests = do
                quickCheck $ counterexample "Adding zero to an expression" prop_Add_0
                quickCheck $ counterexample "Multiplying an expression by one" prop_Mul_1
                quickCheck $ counterexample "Adding an expression with itself" prop_Add_equal
                quickCheck $ counterexample "Add a constant times an expression with another contant times its expressions" prop_Add_equal2
                quickCheck $ counterexample "Subtract an expression from itself" prop_Sub_equal
                quickCheck $ counterexample "Reverse Comparison Test" prop_Rev_Order
                quickCheck $ counterexample "Comparing equal expressions" prop_Compare_Equality
                quickCheck $ counterexample "Comparing unequal expressions" prop_Compare_Inequality
                quickCheck $ counterexample "Additive Commutation between two expressions" prop_Add_Commute
                quickCheck $ counterexample "Multiplicative Commutation between two expressions" prop_Mul_Commute


-- Define the various properties checked by QuickCheck
-- Any function that starts with "prop_" is considered a property by QuickCheck

prop_Add_0 :: Expr Int -> Bool      -- A property of expressions is that adding zero to an expression should result in the same expression
prop_Add_0 e = e + 0 == e
    where types = e::(Expr Int)

prop_Mul_1 :: Expr Int -> Bool
prop_Mul_1 e = e * 1 == e
    where types = e::(Expr Int)

prop_Add_equal :: Expr Int -> Bool
prop_Add_equal e = e + e == (2 * e)
    where types = e :: (Expr Int)

prop_Add_equal2 :: Expr Int -> Bool
prop_Add_equal2 e = (2 * e) + (5 * e) == (7 * e)
    where types = e :: (Expr Int)

prop_Sub_equal :: Expr Int -> Bool
prop_Sub_equal e = (e - e) == 0
    where types = e :: (Expr Int)

prop_Add_Commute :: Expr Int -> Expr Int -> Bool
prop_Add_Commute e1 e2 = e1 + e2 == e2 + e1
    where types = (e1 :: Expr Int, e2 :: Expr Int)

prop_Mul_Commute :: Expr Int -> Expr Int -> Bool
prop_Mul_Commute e1 e2 = e1 * e2 == e2 * e1
    where types = (e1 :: Expr Int, e2 :: Expr Int)

prop_Rev_Order :: Expr Int -> Expr Int -> Bool
prop_Rev_Order e1 e2 = (compare e1 e2) == (flip $ compare e2 e1)
    where types = (e1 :: Expr Int, e2 :: Expr Int)
          flip LT = GT
          flip GT = LT
          flip EQ = EQ

prop_Compare_Equality :: Expr Int -> Bool
prop_Compare_Equality e = (compare e e) == EQ
    where types = e :: (Expr Int)

prop_Compare_Inequality :: Expr Int -> Expr Int -> Bool
prop_Compare_Inequality e1 e2 = if e1 == e2 then (compare e1 e2) == EQ
                                            else (compare e1 e2) /= EQ

-- Since Expr is a custom class we MUST make it an instance of the Arbitrary type-class before we can use it inside QuickCheck properties. The instantiation will let QuickCheck know how to generate random objects of type Expr
-- 'arbitrary' is a definition (it is a function that takes no arguments so it is in effect a constant) which in this context must be of type 'Gen (Expr a)' i.e. an IO which corresponds to a random expression.
-- We define it using the 'sized' function which takes as its single argument a function taking an integer and returning a Gen (Expr a)
-- When we use 'sized' we get access to the size integer that QuickCheck uses to create arbitrary instances. We can use this size value to more intelligently construct the expressions (which is the purpose of arbitrary')

instance (Show a, Integral a) => Arbitrary (Expr a) where
  arbitrary = sized arbitrary'


arbitrary' :: (Show a, Integral a) => Int -> Gen (Expr a)
arbitrary' 0 = arbitrary_const                                  -- Base case which we define to be an arbitrary constant
arbitrary' 1 = oneof [arbitrary_atom, arbitrary_neg_atom]       -- When the required size is 1 we simply return an atomic expression (which can be negative)
arbitrary' n = do
                 ns <- split n
                 assemble ns

                    where
                        assemble (b:bs) = assemble' bs $! arbitrary' b       -- The result of split is never [] (at least a singleton)

                        assemble' [] e = e
                        assemble' (c:cs) e = assemble' cs $! apply op (arbitrary' c) e

                        op = oneof [pure (+), pure (*)]

                        apply o a b = o <*> a <*> b

                        split 0 = pure []
                        split a = do
                                    p <- pick a
                                    fmap (p:) (split (a - p))

                        pick a = choose (1, a)

-- The non-base case uses recursion, monad theory, and applicative functor techniques.
-- Since the result of arbitrary' is the Gen monad (which is analogous to Random) all our calculations must be monadic and so we use 'do' notation.

-- The first task in the 'do' is to use 'split' to construct a random separation of 'n' elements in to parts. 'split' takes an integer 'n' and returns a randomly generated list of integers which all add up to 'n'.
-- It does so recursively. The base case is 'split 0' where we return an empty list.
-- For non-zero 'n' we use 'pick' to get a random integer inside a Gen context. We use '<-' to extract the integer from the Gen context.
-- The next statement inside the 'do' concats the integer to the list inside 'split (a - p)'. Since the recursive call 'split (p - a)' returns a list inside a Gen we use fmap to append 'p' to the list inside the Gen to get a larger list inside the Gen.
-- Since pick returns a monad and split is called from inside a monadic do sequence we are forced to respect the context throughout the calculation.

-- The definition of assemble basically sets it up to use assemble' which performs a "strict" accumulation of the expression as it goes along
-- Note how we take the first integer 'b' and use arbitrary' to create an expression from it. This serves as the initial state of the accumulator. We use $! to force strict evaluation of the result of arbitrary' b to avoid a Stack Overflow

-- The base case of assemble' occurs when the list of integers is empty in which case we simply return the accumulated expression 'e'
-- For the recursive case we apply an operation between arbitrary' c and the expression e which is the accumulator to create the new accumulator. The result of this operation is evaulated strictly and becomes the new accumulator for the recusrive call to assemble' using cs


-- 'apply' is a function which takes three arguments. The first is an operator (* or +) placed inside the Gen context. It is choosen randomly by the definition of 'op'.
-- The definition of 'apply' takes the operator and two arguments and uses applicative functor technique to apply the operation between the two expressions, all of them inside the Gen context (since the whole calculation is Gen monadic).
-- Using currying this means that 'apply op' is a function that takes two Gen monads and returns a Gen monad i.e. its signature is "(Gen a -> Gen a) -> Gen a -> Gen a -> Gen a".


-- Constants and Symbols are the atomic expressions. Everything else is constructed from these (or by encapsulatng them in some fashion).
-- We collect the Const and Symbol expression in to a single arbitrary definition which produces them with equal likelihood
-- This definition will be used to create negative atomic expressions as well.
arbitrary_atom :: (Integral a, Show a) => Gen (Expr a)
arbitrary_atom = oneof [arbitrary_const, arbitrary_symbol]

-- arbitrary_const returns a random Const object by taking a random integer from 1 to 9 and wrapping it inside Const.
-- We don't include 0 because it leaves sums unchanged and more importantly it reduces products to zero which is counter-productive for testing.
-- Negative constants are handled by 'arbitrary_negative' which takes positive constants and negates them.
arbitrary_const :: (Integral a, Show a) => Gen (Expr a)
arbitrary_const = frequency $
                        map (\(f, n) -> (f, return n)) $
                            [(1000, 1), (100, 2), (10, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9)]

-- The constraint 'Integral a' in the signature is crucial since it allows us to use the const' smart constructor to create Const objects from randomly selected Int.

-- We use 'frequency' to change the, well, frequency with which the constants are generated when arbitrary_const is called. Our aim is to have lower integers be more frequently produces than higher ones since it will keep the expressions manageable (verboseCheck lets us know how the distribution is coming out).
-- 'frequency' takes a list of (Int, Gen) tuples where the integer is the weight with which the Gen is produced. So the higher the integer the more likely that Gen will be generated.

-- We first create a list of (Int, Int) tuples where we list the integers 1 to 9 and attach the required weights to them. Highest for 1, then 2, then 3 and the rest are equally weighted at the bottom.
-- The list of tuples for 1,2,3 is created explicitly. The remainder is added to it using ++ and is constructed by taking the list of integers from 4 to 9 and mapping a simple lambda function over it which transforms it in to a list of tuples with frequency 1.

-- We then use map and a lambda function to create Gen (Const Int) objects out of the second element of each tuple using "return n".
-- Note the use of pattern-matching within the lamdba function definition to gain access to the second element.

-- Finally we present the constructed list to frequency for the generation of these objects.



-- Analogous to 'arbitrary_const' this definition, 'arbitrary_symbol', returns a Symbol object corresponding (randomly) to x, y or z.
-- Note that the signature reveals this to be a definition (and not a function). It corresponds to a randomly selected Expr.
arbitrary_symbol :: Gen (Expr a)
arbitrary_symbol = fmap Symbol $ elements ["x", "y", "z"]


-- This definition creates randomly generated negative atomic expressions
arbitrary_neg_atom :: (Show a, Integral a) => Gen (Expr a)
arbitrary_neg_atom = fmap negate arbitrary_atom     -- We map the negate function on the expression inside the Gen returned by arbitrary_atom
