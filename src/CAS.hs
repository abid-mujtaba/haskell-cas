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
-- Author: Abid H. Mujtaba
-- Date: 2015-01-11

-- This module implements a Computer Algebra System for Haskell.
--
-- This work is inspired in part by: https://github.com/hepek/Ramblings/blob/master/symb.lhs


-- The following comment is an instruction to the compiler which gives us access to the ! pattern which is used to require strictness in specified variables
-- {-# LANGUAGE BangPatterns #-}

-- | The CAS module is the top-most entity in this library and gives access to
-- the Expr class and all associated functions that correspond to standard
-- arithmetic operations (+-*/^), .etc.
module CAS                                                           -- A.1
    (
    -- * Classes
    -- | The Expr class is the primary entity forming the foundation of the CAS. We export NONE of the constructors of Expr. The expressions must be built up using either 'symbol' for variable creation OR the defined arithmetic operators
        Expr (),
        symbol              -- Defined in Expr module and exported here. This is the function that allows the user to declare symbolic variables (x, y, z .etc)
    -- * Methods
    --   , (^)                                                          -- A.4
    )
    where

import Expr
import Expr.Show
import Expr.Num
import Expr.Fractional
