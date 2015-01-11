-- Author: Abid H. Mujtaba
-- Date: 2015-01-11

-- This module implements a Computer Algebra System for Haskell.
--
-- This work is heavily inspired by (basically a minor modification of): https://github.com/hepek/Ramblings/blob/master/symb.lhs


-- We start by declaring this .hs file to be module. The module must have the same name as the file (CAS.hs).hs
-- In the module identifier after the module's name and delimited in parentheses we declare the classes, types and functions we want to export from the module.
-- The parentheses are followed by the keyword where and then the rest of the file is dedicated to defining the various objects that are being exported.

module CAS
    (
      Expr(..)                 -- Data typeclass. The .. means ALL its constructors are to be exported
--      , x, y, z, a, b, c
--      , simplify
--      , diff
--      , eval
    )
    where

-- We define the new data typeclass 'Expr' which corresponds to a general algebraic expression. This is achieved by using recursion in the definition of the constructors. By defining these carefully one can use patterm-matching to implement various functions for the 'Expr' typeclass.

data Expr a =                               -- (1)
              Const a                       -- (2)
            | Sum (Expr a) (Expr a)         -- (3)
            deriving (Eq)                   -- (4)

-- (1) -- We declare 'Expr a' to be a type class where 'a' can be any concrete type
-- (2) -- A constant (basically a number) can be an expression by itself (the most basic kind there is)
-- (3) -- This is a recursive constructor with the Constructor name 'Sum' and which takes two 'Expr a' objects as its constructor parameters
-- (4) -- We declare Expr to be an instance of the Eq class since we will want to compare expressions for equality



-- We declare 'Expr' to be an instance of the 'Show' typeclass. Since 'Expr a' is a typeclass with a type-parameter 'a' we declare that in our declaration of the instance we limit ourselves to types of class 'Show' i.e. this instance only refers to types 'Expr a' where 'a' itself is an instance of the Show class. This is the '(Show a) =>' part.
-- Then comes the actual instance declaration 'Show (Expr a)'. We need the parentheses when a type-parameter is included.
-- Lastly comes the 'where' keyword and after it on the following lines comes the functions that must be defined for a type to be an instance of the class. In the case of the 'Show' typeclass this only one function 'show'

instance (Show a) => Show (Expr a) where
  show (Const a) = show a                                           -- (1)
  show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"          -- (2)

-- (1) --  We pattern match on the 'Const a' constructor. In the case of constant we simply show the number. The 'a' in this line is NOT the same as the 'a' in the instance declaration line above it. Here 'a' matches the value inside the 'Const a' constructor. Since the instance declaration limits 'Expr a' to type-parameters 'a' that are an instance of 'Show' so we can run the 'show' method directly on the value 'a' inside the 'Const a' parameter

-- (2) -- This is a recursive function definition where we include parentheses to encapsulate the result. 'show a' and 'show b' are recursive calls which print the two expressions that form part of the 'Sum' constructor