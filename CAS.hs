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
      , x, y, z
--      , simplify
--      , diff
--      , eval
    )
    where

-- We define the new data typeclass 'Expr' which corresponds to a general algebraic expression. This is achieved by using recursion in the definition of the constructors. By defining these carefully one can use patterm-matching to implement various functions for the 'Expr' typeclass.

data Expr a =                               -- (1)
              Const a                       -- (2)
            | Sum (Expr a) (Expr a)         -- (3)
            | Prod (Expr a) (Expr a)
            | Neg (Expr a)
            | Rec (Expr a)                  -- The reciprocal of an expression (1 / Expr)
            | Symbol String                 -- The algebraic variables in the expression: x, y, z, .etc
            deriving (Eq)                   -- (4)

-- (1) -- We declare 'Expr a' to be a type class where 'a' can be any concrete type
-- (2) -- A constant (basically a number) can be an expression by itself (the most basic kind there is)
-- (3) -- This is a recursive constructor with the Constructor name 'Sum' and which takes two 'Expr a' objects as its constructor parameters
-- (4) -- We declare Expr to be an instance of the Eq class since we will want to compare expressions for equality


-- We define and export some useful symbols which will save us time later when we export the module. To that end we simply use the 'Symbol String' constructor.

x = Symbol "x"
y = Symbol "y"
z = Symbol "z"


-- We declare 'Expr' to be an instance of the 'Show' typeclass. Since 'Expr a' is a typeclass with a type-parameter 'a' we declare that in our declaration of the instance we limit ourselves to types of class 'Show' i.e. this instance only refers to types 'Expr a' where 'a' itself is an instance of the Show class. This is the '(Show a) =>' part.
-- Then comes the actual instance declaration 'Show (Expr a)'. We need the parentheses when a type-parameter is included.
-- Lastly comes the 'where' keyword and after it on the following lines comes the functions that must be defined for a type to be an instance of the class. In the case of the 'Show' typeclass this only one function 'show'

instance (Show a) => Show (Expr a) where
  show (Const a) = show a                                           -- (1)
  show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"          -- (2)
  show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Neg a) = '-' : show a                                       -- (3)
  show (Rec a) = "1/" ++ show a
  show (Symbol s) = s                                               -- (4)

-- (1) --  We pattern match on the 'Const a' constructor. In the case of constant we simply show the number. The 'a' in this line is NOT the same as the 'a' in the instance declaration line above it. Here 'a' matches the value inside the 'Const a' constructor. Since the instance declaration limits 'Expr a' to type-parameters 'a' that are an instance of 'Show' so we can run the 'show' method directly on the value 'a' inside the 'Const a' parameter

-- (2) -- This is a recursive function definition where we include parentheses to encapsulate the result. 'show a' and 'show b' are recursive calls which print the two expressions that form part of the 'Sum' constructor

-- (3) -- The negation of an expression is simply appending '-' in front of it. We use the concatenation operator ':' to preprend '-' in front of the String representation of the expression

-- (4) -- Since 's' is a String (from the definition of the 'Symbol' constucor) we don't need to use 'show' here. Had we done so it would have printed the strings surrounded by quotation marks


-- We define Expr to be an instance of the Num class which gives us access to the standard arithematic operations. It is rather evident that we expect the type-parameter 'a' to be an instance of the Num class itself.
-- The really interesting thing is how we will map the arithematic operators on to the constructors of the Expr class. Basically we use the operators to recursivley construct every more complex expressions. This is why we will have to define simplify to carry out obvious simplification of the expression later on.

instance (Integral a) => Num (Expr a) where                 -- (1)
  (+) = Sum                                                 -- (2)
  a - b = Sum a (Neg b)                                     -- (3)
  (*) = Prod
  negate = Neg
  signum = undefined                                        -- (4)
  abs = undefined
  fromInteger a = Const (fromInteger a)

-- (1) -- We want ONLY constant integers in our expressions so we limit the type-constraint from the usual 'Num a' to 'Integral a'. This means that any calls to the methods defined in this class which uses non-integer Constants will raise an exception.

-- (2) -- The operator '+' must be surrounded by parentheses for it to be considered as a normal function and NOT an infix function. Basically 'a + b' is equivalent '(+) a b' where the latter has the advantage of being both curried and in the same format as 'Sum a b'.
       -- We define (+) simply as ths constructor 'Sum' since constructors are actually functions. '(+) = Sum' can also be written as 'a + b = Sum a b' but the shorter notation emphasizes that both '+' and 'Sum' are curried functions.

-- (3) -- This definition is very clear. It however obfuscates the fact that this is a curried function. To see that one can define it equivalently as:
      --            (-) a b = Sum a (Neg b)
      -- And now one can curry it as follows:
      --            let d = (-) (Const 4)
      -- then one can apply 'd' to other expressions:
      --            d x  --->  (4 - x) = Sum (Const 4) (Neg (Symbol "x"))

-- (4) -- The Num typeclass defines a number of functions that we are not interested in implementing since they do not make sense for algebraic expressions. For these we simply define them to be equal to the 'undefined' function which will raise en exception if these are used.

-- (4) -- We define the fromInteger method which gives us the ability to detect integers in expressions from context and convert them in to expressions of type Const. An example will clarify.
       -- With fromInteger so defined we can write '2 * x' and it will be interpreted and converted in to 'Const 2 * Symbol "x"'. This will save us from having to write 'Const 2' all the time.


-- We make Expr an instance of Fractional so we can use the '/' operator.

instance (Integral a) => Fractional (Expr a) where               -- (1)
  a / b = Prod a (Rec b)
  fromRational _ = error "fromRational NOT implemented in Fractional (Expr a): Only integer constants are allowed in Expr."                                                     -- (2)

-- (1) -- The instance declaration has a minor subtlety in the type-constraint 'Integral a'. Usually in such an overloading scenario the type-constraing would be (Fractional a). Note in the definition of the type Expr that the only constructor that (without recursion) uses the type-parameter 'a' is 'Const a'. For our application we are ONLY interested in expressions that contain integer constants so we restrict the type parameter 'a' here to 'Integral a'.
       -- If a non-integer constant is found see -- (2) --

-- (2) -- We define the fromRational method to be an 'error' so that when this method is called the specified error message is printed. This ensures that the constants in our expressions are only allowed to be integers. We will deal with rational fractions by using Prod and Rec.