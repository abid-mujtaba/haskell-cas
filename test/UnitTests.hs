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
-- This module provides accesss to ALL Unit Tests for this project.
-- It exports a single 'main' function that runs all of the unit tests which are defined in sub-modules of UnitTests (inside the test/UnitTests/ folder)
--
-- Source for HUnit usage: https://wiki.haskell.org/HUnit_1.0_User's_Guide


module UnitTests (main)
    where

-- We explicitly import all the functions, types and constructors that we use in THIS module
import Test.HUnit (Test(TestList), Counts, runTestTT)

import qualified UnitTests.Show (tests)
import qualified UnitTests.Addition (tests)
import qualified UnitTests.Multiplication (tests)
-- import qualified UnitTests.Ordering (tests)


main :: IO Counts
main = do                -- This IO Action runs only the unit tests
            runTestTT tests


-- Each sub-module of UnitTests defines a list of TestCase objects called 'tests' (its only export) containing a list of the unit tests it implements
-- We simply concatenate these tests to construct a single 'TestList' which 'main' then executes using the 'runTestTT' function (from Test.HUnit)
tests :: Test
tests = TestList $
            []
        ++ UnitTests.Show.tests
        ++  UnitTests.Addition.tests
        ++  UnitTests.Multiplication.tests
        -- ++  UnitTests.Ordering.tests
