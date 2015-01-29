-- Author: Abid H. Mujtaba
-- Date: 2015-01-11

-- This module implements a Computer Algebra System for Haskell.
--
-- This work is inspired in part by: https://github.com/hepek/Ramblings/blob/master/symb.lhs
--
-- Details comments to this code are given in the accompanying 'comments.md' file. These are labelled using a simple scheme which consists of an uppercase letter followed by a number, e.g. C.3 (search for this label in the comments file to find the relevant comment)

module CAS                                                           -- A.1
    (
      Expr(Symbol)                    -- Data typeclass.             -- A.2
      , x, y, z
      , const'                                                       -- A.3
      , simplify
--      , diff
--      , eval
      , (^)                                                          -- A.4
    )
    where


import Prelude hiding ((^))                 -- P.1
import qualified Prelude                    -- P.2

import Debug.Trace(trace)


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

-- Define the first 10 positive and 9 negative integers for testing.
z0, z1, z2, z3, z4, z5, z6, z7, z8, z9 :: (Integral a) => Expr a
z0 = const' 0
z1 = const' 1
z2 = const' 2
z3 = const' 3
z4 = const' 4
z5 = const' 5
z6 = const' 6
z7 = const' 7
z8 = const' 8
z9 = const' 9

zm1 = const' (-1)
zm2 = const' (-2)
zm3 = const' (-3)
zm4 = const' (-4)
zm5 = const' (-5)
zm6 = const' (-6)
zm7 = const' (-7)
zm8 = const' (-8)
zm9 = const' (-9)

const' :: Integral a => Int -> Expr a                               -- N.1
const' c | c < 0     = Neg (Const $ abs . fromIntegral $ c)         -- N.2
         | otherwise = Const $ fromIntegral c


-- C.1

instance Show a => Show (Expr a) where
  show (Const a)    = show a                                            -- C.2
  show (Sum xs)     = showExprList " + " xs                             -- C.3
  show (Prod xs)    = showExprList " * " xs                             -- ToDo: After implementing sorting we can remove the * symbols like human algebraic notation
  show (Neg a)      = '-' : show a                                      -- C.4
  show (Rec a)      = "1/" ++ show a
  show (Exp a p)    = show a ++ "^" ++ show p
  show (Symbol sym) = sym                                               -- C.5


showExprList :: Show a => String -> [Expr a] -> String                  -- C.6
showExprList _ []   = "(0)"
showExprList sep es = "(" ++ showExprList' sep es ++ ")"

showExprList' :: Show a => String -> [Expr a] -> String                 -- C.7
showExprList' _ []       = ""
showExprList' _ [e]      = show e
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

instance Integral a => Num (Expr a) where                       -- D.2
  a + b     = sum' a b                                          -- D.3
  a - b     = sum' a $ Neg b                                    -- D.4
  (*)       = prod_
  negate    = Neg
  signum    = undefined                                         -- D.5
  abs       = undefined
  fromInteger a
                | a < 0      = Neg (Const (negate $ fromInteger a))
                | otherwise  = Const (fromInteger a)                     -- D.6



-- We make Expr an instance of Fractional so we can use the '/' operator.

instance Integral a => Fractional (Expr a) where               -- E.1
  a / b = a * (Rec b)
  fromRational _ = error "fromRational NOT implemented in Fractional (Expr a): Only integer constants are allowed in Expr."             -- E.2


-- We provide our own definition of the ^ function for exponentiation

(^) :: (Integral a) => Expr a -> Int -> Expr a                                  -- Q.1
a ^ p = s $ exp_ a p                                                            -- Q.2


-- We make Expr an instance of Ord so that we can compare and sort expressions

instance (Ord a, Num a) => Ord (Expr a) where                                 -- L.1
  compare (Const a) (Const b)         = compare a b                           -- L.2
  compare (Const _) (Neg (Const _))   = GT                                    -- L.3
  compare (Neg (Const _)) (Const _)   = LT

  compare (Symbol a) (Symbol b)       = compare a b

  compare (Const _) (Symbol _)        = LT                                    -- L.4
  compare (Symbol _) (Const _)        = GT

  compare (Neg a) (Neg b)             = compare a b                           -- L.5
  compare a (Neg b)                   = compare a b
  compare (Neg a) b                   = compare a b

  compare (Rec a) (Rec b)             = compare b a
  compare _ (Rec _)                   = GT                                    -- L.6
  compare (Rec _) _                   = LT

  compare a b = compareDegree a b                                             -- L.7


-- A function for doing a degree based comparison of two expressions:
compareDegree :: (Ord a, Num a) => Expr a -> Expr a -> Ordering
compareDegree (Exp a pa) (Exp b pb)                                           -- L.8
                                | pa == pb  = compare a b
                                | da < db   = LT
                                | da > db   = GT
                                | otherwise = undefined
                                    where
                                        da = pa * degree a
                                        db = pb * degree b
-- ToDo compare exponent with non-exponent expressions
compareDegree a b                                                             -- L.9
            | da < db       = LT
            | da > db       = GT
            | otherwise     = compare' a b
                where
                    da = degree a
                    db = degree b


-- A function for comparing expressions with equal degree
compare' :: (Ord a, Num a) => Expr a -> Expr a -> Ordering
compare' a b = undefined                                                     -- L.10



-- Calculate the degree of an expression (polynomial)
degree :: Num a => Expr a -> Int                                  -- O.1
degree (Const _)   = 0
degree (Symbol _)  = 1
degree (Neg e)     = degree e
degree (Rec e)     = negate $ degree e
degree (Prod xs)   = sum $ map degree xs                          -- O.2
degree (Sum xs)    = foldl1 max $ map degree xs                   -- O.3
degree (Exp e pwr) = pwr * degree e



-- We define functions that intelligently carry out the various arithematic operations.

sum' :: Integral a => Expr a -> Expr a -> Expr a
sum' (Sum xs) (Sum ys)   = s . Sum $ xs ++ ys                   -- F.1
sum' n (Sum ns)          = s . Sum $ n:ns                       -- F.2
sum' (Sum ns) n          = s . Sum $ ns ++ [n]
sum' m n                                                        -- F.3
        | m == n         = s $ (2 * m)
        | otherwise      = s $ Sum [m, n]



-- Multiplying expressions

prod_ :: Integral a => Expr a -> Expr a -> Expr a                                   -- R.1

prod_ (Neg a) (Neg b)   = prod_ a b
prod_ (Neg a) b         = Neg (prod_ a b)
prod_ a (Neg b)         = Neg (prod_ a b)

prod_ (Const 0) _       = Const 0
prod_ _ (Const 0)       = Const 0
prod_ (Const 1) e       = e
prod_ e (Const 1)       = e

prod_ epa@(Exp ea pa) epb@(Exp eb pb)                           -- R.2
            | ea == eb      = exp' ea (pa + pb)
            | otherwise     = prod' epa epb

prod_ epa@(Exp ea pa) eb                                        -- R.3
            | ea == eb      = exp' ea (pa + 1)
            | otherwise     = prod' epa eb

prod_ ea epb@(Exp _ _)    = prod_ epb ea                        -- R.4

-- Rules for multiplying Prod with other expressions
prod_ (Prod [e]) p2@(Prod _)       = prod_ e p2                             -- R.5
prod_ (Prod (e:es)) p2@(Prod _)    = prod_ (Prod es) (prod_ e p2)

prod_ a b@(Prod ps) = branch $ match a ps                                           -- R.6
                        where
                            branch Nothing      = prod' a b                         -- R.6a
                            branch (Just es)    = Prod es

                            match _ []            = Nothing                         -- R.6b

                            match c (ea@(Exp d p):es)                               -- R.6c
                                    | c == d      = Just $ (exp' d (p+1)):es
                                    | otherwise   = fmap (ea:) (match c es)

                            match c (e:es)                                          -- R.6d
                                    | c == e      = Just $ (exp' c 2):es
                                    | otherwise   = fmap (e:) (match c es)

prod_ ea eb                                             -- R.7
        | ea == eb      = exp_ ea 2
        | otherwise     = prod' ea eb



prod' :: Integral a => Expr a -> Expr a -> Expr a       -- T.1

prod' c@(Const _) e     = prod_c c e                    -- T.2
prod' sym@(Symbol _) e  = prod_s sym e



-- Rules for multiplying Exp with other expressions
prod' ea@(Exp (Symbol a) _) eb@(Exp (Symbol b) _)                                  -- R.9
                                | a < b     = Prod [ea, eb]
                                | otherwise = Prod [eb, ea]

prod' ea@(Exp (Symbol _) _) eb@(Exp _ _)   = Prod [ea, eb]                          -- R.10a
prod' ea@(Exp _ _) eb@(Exp (Symbol _) _)   = Prod [eb, ea]

prod' ea@(Exp _ _) r@(Rec _)               = Prod [r, ea]
prod' ea@(Exp (Symbol _) _) sm@(Sum _)     = Prod [ea, sm]                          -- R.10b

prod' ea@(Exp (Sum _) _) sm@(Sum _)     = Prod [sm, ea]                             -- R.10c

prod' ea@(Exp (Symbol a) n) (Prod ps)   = Prod $ mul_exp a n ps                                                     -- R.20
                                            where
                                                mul_exp b p (c@(Const _):es)    = c:(mul_exp b p es)
                                                mul_exp b p (r@(Rec _): es)     = r:(mul_exp b p es)

                                                mul_exp b p (sc@(Symbol c):es)
                                                                | b < c     = ea:sc:es
                                                                | b > c     = sc:(mul_exp b p es)
                                                                | otherwise = (exp' (Symbol b) (p+1)):es

                                                mul_exp b p (ec@(Exp (Symbol c) pc):es)
                                                                | b < c     = ea:ec:es
                                                                | b > c     = ec:(mul_exp b p es)
                                                                | otherwise = (exp' (Symbol b) (p + pc)):es

                                                mul_exp _ _ es              = ea:es

prod' ea@(Exp _ _) (Prod ps)    = Prod $ ps ++ [ea]                     -- ToDo: Deal with case of repeated expression in multiplication (ea exits inside ps as well) (x + y)^2 * (3 * (x + y)^4)


-- Rules for multiplyng Rec with other expressions
prod' (Rec ra) (Rec rb)                  = Rec (prod' ra rb)            -- ToDo: Implement cancellation.
prod' r@(Rec _) sm@(Sum _)               = Prod [r, sm]
prod' rc@(Rec _) (Prod (rp@(Rec _):es))  = multiply $ (prod' rc rp):es                  -- R.23
                                                where
                                                    multiply [e]    = e
                                                    multiply (a:as) = prod' a (multiply as)
prod' rc@(Rec _) (Prod es)               = Prod $ rc:es                                -- R.24


-- Rules for multiplying Sum with other expressions
prod' sa@(Sum _) sb@(Sum _) = Prod [sa, sb]                                            -- R.25

prod' sa@(Sum _) (Prod ps) = Prod $ mul_sum sa ps                                      -- R.26
                                where
                                    mul_sum sb (sc@(Sum _):es)
                                                | sb == sc      = (exp' sb 2):es       -- ToDo: Handle this generically in prod_
                                                | otherwise     = sc:(mul_sum sb es)

                                    mul_sum sb (ec@(Exp sc@(Sum _) p):es)
                                                | sb == sc      = (exp' sb (p + 1)):es  -- ToDo: Handle this generically in prod_
                                                | otherwise     = ec:(mul_sum sb es)

                                    mul_sum sb (e:es)   = e:(mul_sum sb es)


-- Catch-all rule that implements commutation
prod' m n                 = prod' n m                                               -- R.22



-- Rules for multiplying Const with other expressions
prod_c :: Integral a => Expr a -> Expr a -> Expr a                              -- U.1

prod_c (Const a) (Const b)               = Const (a*b)
prod_c a@(Const _) sym@(Symbol _)        = Prod [a, sym]                        -- U.2
prod_c a@(Const _) b@(Exp _ _)           = Prod [a, b]                          -- U.3
prod_c a@(Const _) b@(Rec _)             = Prod [b, a]                          -- U.4
prod_c a@(Const _) b@(Sum _)             = Prod [a, b]

prod_c c@(Const v) (Prod ps) = Prod $ mul v ps                                           -- U.5
                                where
                                    mul a ((Const b):es)        = Const (a * b):es       -- U.5a
                                    mul a (r@(Rec _):es)        = r:(mul a es)           -- U.5b     -- ToDo: Implement cancellation
                                    mul _ es                    = c:es                   -- U.5c

prod_c _ _ = error "prod_c is only intended for multiplying Const with other expressions"           -- U.6


-- Rules for multiplying Symbols with other expressions
prod_s :: Integral a => Expr a -> Expr a -> Expr a

prod_s sa@(Symbol a) sb@(Symbol b)                                               -- V.1
                                | a < b      = Prod [sa, sb]
                                | otherwise  = Prod [sb, sa]

prod_s sa@(Symbol a) eb@(Exp (Symbol b) _)                                       -- V.2
                                        | a < b      = Prod [sa, eb]
                                        | otherwise  = Prod [eb, sa]

prod_s sa@(Symbol _) e@(Exp _ _ )    = Prod [sa, e]                              -- V.3
prod_s sa@(Symbol _) r@(Rec _)       = Prod [r, sa]                              -- ToDo: Deal with cancellation
prod_s sa@(Symbol _) ss@(Sum _)      = Prod [sa, ss]

prod_s sa@(Symbol a) (Prod ps)       = Prod $ mul a ps                                                            -- V.4
                                            where
                                                mul b (c@(Const _):es)      = c:(mul b es)                        -- V.5
                                                -- ToDo: Add functionality for cancellation of symbol with Rec
                                                mul _ (r@(Rec _):es)        = r:(mul a es)

                                                mul b (sc@(Symbol c):es)                                          -- V.6
                                                                | b < c      = sa:sc:es
                                                                | otherwise  = sc:(mul b es)

                                                mul b (e@(Exp (Symbol c) _):es)                                   -- V.7
                                                                | b < c      = sa:e:es
                                                                | otherwise  = e:(mul b es)

                                                mul _ es                     = sa:es                              -- V.8

prod_s _ _ = error "prod_s is only intended for multiplying Symbol expression"



-- Exponentiation of expressions

exp_ :: Integral a => Expr a -> Int -> Expr a
exp_ e p                                                        -- S.1
    | p > 0     = exp' e p
    | p < 0     = Rec (exp' e $ abs p)                   -- ToDo: Implement a rec' function for smart reciprocal of expressions including Rec (Rec _)
    | otherwise = Const 1


exp':: Integral a => Expr a -> Int -> Expr a

exp' (Neg e) p                                                  -- S.2
        | odd p     = Neg (exp' e p)
        | otherwise = exp' e p

exp' (Rec e) p      = Rec (exp' e p)                            -- S.3

exp' _ 0 = Const 1
exp' e 1 = e

exp' (Const c) p     = Const (c Prelude.^ p)                    -- S.4
exp' (Exp e p) q     = Exp e (p * q)
exp' e p             = Exp e p                                  -- S.5


-- Let us define simplification methods.

s :: Integral a => Expr a -> Expr a                   -- Takes an expression and returns a simplified expression.

--s o@(Const c) | c < 0           = Neg (Const $ negate c)                                -- G.1
--              | otherwise       = o
--
--s (Exp (Const c) p)             = Const $ (Prelude.^) c p                               -- G.2
--s (Exp (Neg (Const c)) p)       = s . Const $ (Prelude.^) (negate c) p                  -- G.3

s (Sum xs)  = simplify_sum xs                                                           -- G.4

-- Turned off while we develop a more elegent    approach to the creation of Prod expressions
--s (Prod xs) = simplify_prod xs

s e         = e                                                                         -- G.5



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
