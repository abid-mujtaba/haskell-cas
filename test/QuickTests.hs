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
-- This module contains ALL Quick Tests (ones using randomly generated test data) for this project.
-- It exports a single 'main' function that runs all of the tests defined here-in
--
-- Source for QuickCheck usage: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

module QuickTests (main)
    where

import Control.Applicative
import Test.QuickCheck
-- import Debug.Trace(trace,traceShow)

import CAS


main:: IO ()
main= do               -- This IO Action runs only the property checks
            quickTests

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
