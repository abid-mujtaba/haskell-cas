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
-- This module contains ALL Unit Tests for this project.
-- It exports a single 'main' function that runs all of the tests defined here-in
--
-- Source for HUnit usage: https://wiki.haskell.org/HUnit_1.0_User's_Guide


module UnitTests.Main (main)
    where

import Prelude hiding ((^))     -- This allows us to use the ^ operator defined in CAS without collision with Prelude.^

import qualified UnitTests.Addition (tests)
import qualified UnitTests.Multiplication (tests)
import qualified UnitTests.Ordering (tests)


main :: IO Counts
main = do                -- This IO Action runs only the unit tests
            runTestTT tests


-- Each sub-module of UnitTests defines a list of TestCase objects called 'tests' (its only export) containing a list of the unit tests it implements
-- We simply concatenate these tests to construct a single 'TestList' which 'main' then executes using the 'runTestTT' function (from Test.HUnit)
tests :: Test
tests = TestList $
            UnitTests.Addition.tests
        ++  UnitTests.Multiplication.tests
        ++  UnitTests.Ordering.tests
