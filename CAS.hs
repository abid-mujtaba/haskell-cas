-- Author: Abid H. Mujtaba
-- Date: 2015-01-11

-- This module implements a Computer Algebra System for Haskell.
--
-- This work is inspired in part by: https://github.com/hepek/Ramblings/blob/master/symb.lhs
--
-- Details comments to this code are given in the accompanying 'comments.md' file. These are labelled using a simple scheme which consists of an uppercase letter followed by a number, e.g. C.3 (search for this label in the comments file to find the relevant comment)

module CAS                      -- A.1
    (
      Expr(..)                 -- Data typeclass. The .. means ALL its constructors are to be exported
      , x, y, z
      , z0, z1, z2, z3, z4, z5, z6, z7, z8, z9          -- Used for testing. Will be removed later.
      , simplify
--      , diff
--      , eval
    )
    where



data Expr a =                               -- B.1, B.2
              Const a                       -- B.3
            | Sum [Expr a]                  -- B.4
            | Prod (Expr a) (Expr a)
            | Neg (Expr a)
            | Rec (Expr a)                  -- B.5
            | Exp (Expr a) Int              -- B.6
            | Symbol String                 -- B.7
            deriving (Eq)                   -- B.8


-- We define and export some useful symbols which will save us time later when we export the module. To that end we simply use the 'Symbol String' constructor.

x, y, z :: Expr a
x = Symbol "x"
y = Symbol "y"
z = Symbol "z"

-- Define the first 10 integers for testing.
z0, z1, z2, z3, z4, z5, z6, z7, z8, z9 :: (Integral a) => Expr a
z0 = Const 0
z1 = Const 1
z2 = Const 2
z3 = Const 3
z4 = Const 4
z5 = Const 5
z6 = Const 6
z7 = Const 7
z8 = Const 8
z9 = Const 9


-- C.1

instance (Show a) => Show (Expr a) where
  show (Const a) = show a                                           -- C.2
  show (Sum xs) = showExprList " + " xs                             -- C.3
  show (Prod a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Neg a) = '-' : show a                                       -- C.4
  show (Rec a) = "1/" ++ show a
  show (Exp a p) = show a ++ "^" ++ show p
  show (Symbol s) = s                                               -- C.5


showExprList :: Show a => String -> [Expr a] -> String              -- C.6
showExprList _ [] = "(0)"
showExprList sep es = "(" ++ showExprList' sep es ++ ")"

showExprList' :: Show a => String -> [Expr a] -> String             -- C.7
showExprList' _ [] = ""
showExprList' _ [e] = show e
showExprList' sep (e:es) = show e ++ sep ++ showExprList' sep es



-- D.1

instance (Integral a) => Num (Expr a) where                 -- D.2
  a + b = sum' a b                                          -- D.3
  a - b = sum' a $ Neg b                                    -- D.4
  (*) = Prod
  negate = Neg
  signum = undefined                                        -- D.5
  abs = undefined
  fromInteger a = Const (fromInteger a)                     -- D.6



-- We make Expr an instance of Fractional so we can use the '/' operator.

instance (Integral a) => Fractional (Expr a) where               -- (1)
  a / b = Prod a (Rec b)
  fromRational _ = error "fromRational NOT implemented in Fractional (Expr a): Only integer constants are allowed in Expr."                                                     -- (2)

-- (1) -- The instance declaration has a minor subtlety in the type-constraint 'Integral a'. Usually in such an overloading scenario the type-constraing would be (Fractional a). Note in the definition of the type Expr that the only constructor that (without recursion) uses the type-parameter 'a' is 'Const a'. For our application we are ONLY interested in expressions that contain integer constants so we restrict the type parameter 'a' here to 'Integral a'.
       -- If a non-integer constant is found see -- (2) --

-- (2) -- We define the fromRational method to be an 'error' so that when this method is called the specified error message is printed. This ensures that the constants in our expressions are only allowed to be integers. We will deal with rational fractions by using Prod and Rec.


-- We define functions that intelligently carry out the various arithematic operations.

sum' :: (Integral a) => Expr a -> Expr a -> Expr a
sum' (Sum xs) (Sum ys)   = Sum $ xs ++ ys                   -- (1)
sum' n (Sum ns)          = Sum $ n:ns                       -- (2)
sum' (Sum ns) n          = Sum $ ns ++ [n]
sum' m n                 = Sum [m, n]                        -- (3)

-- (1) -- We are carrying out pattern-matching where the first pattern that matches is the one that is used. So our first pattern matches the addition of two Sum expressions with xs and ys matching the lists encapsulated by both. The result is simply another Sum with the two lists concatenated.

-- (2) -- The next two patterns match the possibility of a single Sum expression being added to a non-sum expression (since the case of two Sum-s being added corresponds to the pattern at the top). In such a case we simply add the expression to the list within the Sum.

-- (3) -- The final pattern which is the catch-all corresponds by elimination (via the upper patterns) to the case of two non-Sum expression being added. In this case we simply construct a list of two elements and put it inside the Sum.


-- Let us define simplification methods.

s :: (Integral a) => Expr a -> Expr a                   -- Takes an expression and returns a simplified expression.
s (Sum xs) = empty_sum $ simplify_sum xs                --(1)

-- (1) -- We define the simplification for a Sum expression that encapsulates a list of expressions.
       -- We use pattern matching on the RHS to get the list of expressions 'xs'
       -- On the RHS we first use simplify_sum on the list of expressions. Then we pass the result on to 'empty_sum' which tests for the possibility of an empty list.


--s o@(Sum a b) | a == b = Prod 2 a                                           -- (2)
--              | otherwise = o

-- (1) -- We use the 'let' keyword to bind the result of the calculation 'a - b' to the name 'c'. Then we check if c > 0 and based on that return the appropriate result.

-- (2) -- Here we use an as-pattern to bind the name 'o' to the pattern we are matching. In this case since it is at the end this constitutes the catch-all pattern.
       -- Then we use guards to determine the result. If a == b then a + b = a + a = 2 a.
       -- If a != b then we simply return the matched pattern (the original Sum) using the name 'o' which we had bound to the original pattern.


-- We define simplification method for the list of expressions inside a Sum.
simplify_sum :: (Integral a) => [Expr a] -> [Expr a]
simplify_sum xs = collect_const xs

-- We define a utility function for collecting Const terms inside a list of expressions which are intended for encapsulation in a Sum.
collect_const :: (Integral a) => [Expr a] -> [Expr a]
collect_const xs = let (c, es) = foldr fold_constants (0, []) xs in           -- (1)
                        if c == 0 then es
                        else es ++ [Const c]

-- (1) -- We use a let expression to bind (c, es) to the result of the foldr. We use this binding in the if statement that follows. If after the foldr the collected constant value is 0 we simply return the collected list 'es'.
       -- If c != 0 then we simply append a Const expression corresponding to 'c' at the end of the list of expressions 'es'
       -- The foldr takes a binary function, an initial accumulator which in this case is the tuple (0, []) and then folds the function over the list of expressions.
       -- The idea is that the current accumulator and one element of the list is fed to the binary function 'fold_constants'. The function analyzses the element. If the element is a Const then its value is added to the first member of the accumulator which keeps track of the sum of constant values.
       -- If the element passed in to fold_constants is NOT a Const then we leave the sum value unchanged and append the element to the list of expressions which forms the second part of the accumulator.
       -- With this in mind it is obvious that the initial accumulator which appears in 'foldr' must be (0, []) because we start with a constants sum value of 0 (and add to it element by element) and we start with an empty list to which we append non-Const expressions as we fold over the list 'xs'
       -- By using a foldr here we get to keep the order of elements from the original list

-- Write a binary function which we will use inside the foldr for collecting constants.
fold_constants :: (Integral a) => Expr a -> (a, [Expr a]) -> (a, [Expr a])      -- (1)
fold_constants e (m, xs) = case e of Const n -> ((m + n), xs)                   -- (2)
                                     Neg (Const n) -> ((m - n), xs)             -- (3)
                                     _ -> (m, e:xs)                             -- (4)

-- (1) -- The desired functionality for this binary function is defined in comment (1) for the 'collect_const' function.
       -- Because it is to be used in a 'foldr' (as contrasted with a 'foldl') the element of the folded list is the first argument to 'foldr' and the accumulator is the second.
       -- Consequently the signature starts with an Expr a for the element and then (a, [Expr a]) for the accumulator and returns an object of the same type as the accumulator
       -- Note: By using a type-constraing of 'Integral a' we can refer to integers inside 'Const n'. We use (a, [Expr a]) because we want the first element of the accumulator to have the same type as that encapsualted by the Const. It should be further noted that the definition of Expr shows that the only value constructor of Expr that uses the type-parameter 'a' in its definition is 'Const'

-- (2) -- We use pattern matching to access the element 'e', accumulator sum value 'm' and the collected list of expressions 'xs'
       -- On the RHS we use a case expression to pattern-match on the element 'e' which is of type 'Expr a'
       -- The first pattern we match is for 'e' being a Const with value n. In this case we simply add n to the current accumulator value m and leave the list of expressions unchanged (basically removing the Const from the list and placing its value inside the sum integer)

-- (3) -- The second pattern matches for a negative constant and behave analogous to the first pattern.

-- (4) -- If the element 'e' is not a (negative) constant we leave the sum value 'm' unchanged and prepend the element to the collected list. By virtue of prepending and not appending in a function that is used in a 'foldr' we retain the ordering of the original list.


-- A simple function for dealing with the possibility of a sum with no expressions inside
empty_sum :: Integral a => [Expr a] -> Expr a
empty_sum xs = case xs of [] -> Const 0                     -- (1)
                          [e] -> e                          -- (2)
                          _ -> Sum xs

-- (1) -- If the list of expressions destined to be encapsulated by Sum is empty (tested using the 'null' function) we return a Const 0 which is the logical equivalent of an empty sum (e.g. 3 + -3 = 0).

-- (2) -- If the list contains a single expression then we return just that expression (e.g. x + 0 = x). Otherwise we return the list encapsulated by a Sum


-- We implement a full simplification method which we export.
-- For now this method simply equals the 's' method we have defined above.

simplify :: (Integral a) => Expr a -> Expr a
simplify = s
