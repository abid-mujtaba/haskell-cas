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
-- It exports a single 'main' function that runs all of the tests defined in its sub-modules
--
-- Source for QuickCheck usage: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

module QuickTests (main)
    where

import qualified QuickTests.Addition (tests)
import qualified QuickTests.Multiplication (tests)
import qualified QuickTests.Ordering (tests)


main:: IO ()
main = do               -- This IO Action runs only the property checks
            QuickTests.Addition.tests
            QuickTests.Multiplication.tests
            QuickTests.Ordering.tests
