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
-- The Stack build-system provides an automated way for running these tests:
--
-- stack test


import qualified UnitTests.Main              -- All Unit tests are defined in the UnitTests module. We use a qualified import to keep the 'main' function inside it in a separate namespace
import qualified QuickTests


main = do                     -- In the main function we simply run the tests. So running the executable (Test) will cause the tests to be executed
          putStrLn "\n\n============ Unit Tests ===============\n"
          UnitTests.Main.main

        --   putStrLn "\n=========== Random Tests ===============\n"
        --   QuickTests.main
