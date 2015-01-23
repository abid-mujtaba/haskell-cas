-- Author: Abid H. Mujtaba
-- Date: 2015-01-11

-- This module implements a Computer Algebra System for Haskell.
--
-- This work is inspired in part by: https://github.com/hepek/Ramblings/blob/master/symb.lhs
--
-- Details comments to this code are given in the accompanying 'comments.md' file. These are labelled using a simple scheme which consists of an uppercase letter followed by a number, e.g. C.3 (search for this label in the comments file to find the relevant comment)

module CAS                                                           -- A.1
    (
      Expr(Const, Symbol)                    -- Data typeclass.      -- A.2
      , x, y, z
      , simplify
--      , diff
--      , eval
    )
    where



data Expr a =                               -- B.1, B.2
              Const a                       -- B.3
            | Sum [Expr a]                  -- B.4
            | Prod [Expr a]
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

instance Show a => Show (Expr a) where
  show (Const a) = show a                                           -- C.2
  show (Sum xs) = showExprList " + " xs                             -- C.3
  show (Prod xs) = showExprList " * " xs                            -- ToDo: After implementing sorting we can remove the * symbols like human algebraic notation
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


-- Utility (Debugging) method for printing out the expression as it really is (in terms of its Constructors)
showActual :: Show a => Expr a -> String
showActual (Const c)    = "Const " ++ show c                                                                -- M.1
showActual (Symbol sym) = sym                                                                               -- M.2
showActual (Neg e)      = "Neg (" ++ showActual e ++ ")"                                                    -- M.3
showActual (Rec e)      = "Rec (" ++ showActual e ++ ")"
showActual (Exp e p)    = "Exp (" ++ showActual e ++ ")(" ++ show p ++ ")"
showActual (Sum xs)     = "Sum [" ++ (drop 2 $ foldl foldListElement "" xs) ++ "]"                          -- M.4
showActual (Prod xs)    = "Prod [" ++ (drop 2 $ foldl foldListElement "" xs) ++ "]"


-- Binary function that is used to show a list of expressions. It is intended for use in a fold.
foldListElement :: Show a => String -> Expr a -> String
foldListElement acc e = acc ++ ", " ++ showActual e                                                         -- M.5



-- D.1

instance Integral a => Num (Expr a) where                   -- D.2
  a + b = sum' a b                                          -- D.3
  a - b = sum' a $ Neg b                                    -- D.4
  (*) = prod'
  negate = Neg
  signum = undefined                                        -- D.5
  abs = undefined
  fromInteger a
                | a < 0                = Neg (Const (abs $ fromInteger a))
                | otherwise            = Const (fromInteger a)                     -- D.6



-- We make Expr an instance of Fractional so we can use the '/' operator.

instance Integral a => Fractional (Expr a) where               -- E.1
  a / b = a * (Rec b)
  fromRational _ = error "fromRational NOT implemented in Fractional (Expr a): Only integer constants are allowed in Expr."             -- E.2


-- We make Expr an instance of Ord so that we can compare and sort expressions

instance Ord a => Ord (Expr a) where                              -- L.1
  compare (Const a) (Const b)       = compare a b                 -- L.2
  compare (Const _) (Neg (Const _)) = GT                          -- L.3
  compare (Neg (Const _)) (Const _) = LT
  compare (Symbol a) (Symbol b) = compare a b
  compare a b = undefined


-- Calculate the degree of an expression (polynomial)
degree :: Num a => Expr a -> Int                                  -- L.3
degree (Const _)   = 0
degree (Symbol _)  = 1
degree (Neg e)     = degree e
degree (Rec e)     = negate $ degree e
degree (Prod xs)   = sum $ map degree xs                          -- L.4
degree (Sum xs)    = foldl1 max $ map degree xs                   -- L.5
degree (Exp e pwr) = pwr * degree e



-- We define functions that intelligently carry out the various arithematic operations.

sum' :: Integral a => Expr a -> Expr a -> Expr a
sum' (Sum xs) (Sum ys)   = s . Sum $ xs ++ ys                   -- F.1
sum' n (Sum ns)          = s . Sum $ n:ns                       -- F.2
sum' (Sum ns) n          = s . Sum $ ns ++ [n]
sum' m n                                                        -- F.3
        | m == n         = s $ (2 * m)
        | otherwise      = s $ Sum [m, n]


prod' :: Integral a => Expr a -> Expr a -> Expr a               -- F.4
prod' (Prod xs) (Prod ys) = s . Prod $ xs ++ ys
prod' n (Prod ns)         = s . Prod $ n:ns
prod' (Prod ns) n         = s . Prod $ ns ++ [n]
prod' m n                 = s $ Prod [m, n]


-- Let us define simplification methods.

s :: Integral a => Expr a -> Expr a                   -- Takes an expression and returns a simplified expression.
s (Sum xs)  = simplify_sum xs                         -- G.1
s (Prod xs) = simplify_prod xs



-- We define the simplification method for the list of expressions inside a Sum.
simplify_sum :: Integral a => [Expr a] -> Expr a
simplify_sum xs = empty_sum $ collect_sum_const xs


-- We define a utility function for collecting Const terms inside a list of expressions which are intended for encapsulation in a Sum.
collect_sum_const :: Integral a => [Expr a] -> [Expr a]
collect_sum_const xs = let (c, es) = foldr fold_sum_constants (0, []) xs in                                             -- H.1
                            append_constant c es                                                                        -- H.2
                                    where append_constant c es | c == 0    = es                                         -- H.3
                                                               | c > 0     = es ++ [Const c]
                                                               | otherwise = es ++ [Neg (Const (abs c))]


-- Write a binary function which we will use inside the foldr for collecting constants.
fold_sum_constants :: Integral a => Expr a -> (a, [Expr a]) -> (a, [Expr a])        -- I.1
fold_sum_constants e (m, es) = case e of Const n -> ((m + n), es)                   -- I.2
                                         Neg (Const n) -> ((m - n), es)             -- I.3
                                         _ -> (m, e:es)                             -- I.4


-- A simple function for dealing with the possibility of a sum with no expressions inside
empty_sum :: Integral a => [Expr a] -> Expr a
empty_sum xs = case xs of [] -> Const 0                     -- J.1
                          [e] -> e                          -- J.2
                          _ -> Sum xs



-- We define the simplification method (and its utility methods) for the list of expressions inside a Product.
simplify_prod :: Integral a => [Expr a] -> Expr a
simplify_prod xs = single_prod $ collect_prod_const xs


-- A utility function for collecting the Const terms inside a list of expressions intended for encapsulation by a Prod.
collect_prod_const :: Integral a => [Expr a] -> [Expr a]
collect_prod_const xs = let (n, d, es) = foldr fold_prod_constants (1, 1, []) xs in             -- K.1
                            case (n, d) of (1, 1) -> es                                         -- K.2
                                           (1, _) -> (Rec (Const d)):es
                                           (_, 1) -> (Const n):es
                                           _      -> (Const n):(Rec (Const d)):es


-- A binary function which is used inside the foldr for collecting constants
fold_prod_constants :: Integral a => Expr a -> (a, a, [Expr a]) -> (a, a, [Expr a])
fold_prod_constants e (n, d, es) = case e of Const m -> (n * m, d, es)                          -- K.3
                                             Rec (Const m) -> (n, d * m, es)
                                             _ -> (n, d, e:es)


-- A simple function for dealing with the possibility of a product with just one expression
single_prod :: Integral a => [Expr a] -> Expr a
single_prod xs = case xs of [] -> Const 1                                                       -- K.4
                            [e] -> e
                            _ -> Prod xs



-- We implement a full simplification method which we export.
-- For now this method simply equals the 's' method we have defined above.

simplify :: Integral a => Expr a -> Expr a
simplify = s
