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
--
-- Details comments to this code are given in the accompanying 'comments.md' file. These are labelled using a simple scheme which consists of an uppercase letter followed by a number, e.g. C.3 (search for this label in the comments file to find the relevant comment)

-- The following comment is an instruction to the compiler which gives us access to the ! pattern which is used to require strictness is specified variables
{-# LANGUAGE BangPatterns #-}

module CAS                                                           -- A.1
    (
      Expr(Symbol)                    -- Data typeclass.             -- A.2
--      , simplify
--      , diff
--      , eval
      , (^)                                                          -- A.4
    )
    where


import Prelude hiding ((^))                 -- P.1
import qualified Prelude                    -- P.2
import Data.Monoid(mappend)                 -- P.3
import Data.List(delete)

import Debug.Trace(trace)


data Expr a =                               -- B.1, B.2
              Const a                       -- B.3
            | Sum [Expr a]                  -- B.4
            | Prod [Expr a]
            | Neg (Expr a)
            | Frac (Expr a) (Expr a)        -- B.5
            | Exp (Expr a) Int              -- B.6
            | Symbol String                 -- B.7
            deriving (Eq)                   -- B.8



const' :: Integral a => Int -> Expr a                               -- N.1
const' c | c < 0     = Neg (Const $ negate . fromIntegral $ c)         -- N.2
         | otherwise = Const $ fromIntegral c


-- C.1

instance Show a => Show (Expr a) where
  show (Const a)    = show a                                            -- C.2
  show (Sum xs)     = "(" ++ showSum xs ++ ")"
  show (Prod xs)    = showExprList " * " xs
  show (Neg a)      = '-' : show a                                      -- C.4
  show (Frac n d)   = show n ++ "/" ++ show d
  show (Exp a p)    = show a ++ "^" ++ show p
  show (Symbol sym) = sym                                               -- C.5


showExprList :: Show a => String -> [Expr a] -> String                  -- C.6
showExprList _ []   = error "Shouldn't have to show an empty product"
showExprList sep es = "(" ++ showExprList' sep es ++ ")"

showExprList' :: Show a => String -> [Expr a] -> String                 -- C.7
showExprList' _ []       = error "Shouldn't have to show an empty list of expressions"
showExprList' _ [e]      = show e
showExprList' sep (e:es) = show e ++ sep ++ showExprList' sep es

showSum :: Show a => [Expr a] -> String
showSum []                      = error "Shouldn't have to show an empty sum"
showSum [e]                     = show e
showSum (e : ( (Neg f):es ))    = show e ++ " - " ++ showSum (f:es)     -- C.8
showSum (e : es)                = show e ++ " + " ++ showSum es


-- Utility (Debugging) method for printing out the expression as it really is (in terms of its Constructors)
showActual :: Show a => Expr a -> String
showActual (Const c)    = "Const " ++ show c                                                                -- M.1
showActual (Symbol sym) = sym                                                                               -- M.2
showActual (Neg e)      = "Neg (" ++ showActual e ++ ")"                                                    -- M.3
showActual (Frac a b)   = "Frac (" ++ showActual a ++ ")(" ++ showActual b ++ ")"
showActual (Exp e p)    = "Exp (" ++ showActual e ++ ")(" ++ show p ++ ")"
showActual (Sum xs)     = "Sum [" ++ (drop 2 $ foldl foldListElement "" xs) ++ "]"                         -- M.4
showActual (Prod xs)    = "Prod [" ++ (drop 2 $ foldl foldListElement "" xs) ++ "]"


-- Binary function that is used to show a list of expressions. It is intended for use in a fold.
foldListElement :: Show a => String -> Expr a -> String
foldListElement acc e = acc ++ ", " ++ showActual e                                                         -- M.5



-- ToDo: Remove dependance on Show
instance (Show a, Integral a) => Num (Expr a) where                       -- D.1 --D.2
  a + b     = sum_ a b                                          -- D.3
  a - b     = sum_ a $ neg' b                                   -- D.4
  (*)       = prod_
  negate    = neg'
  signum    = undefined                                         -- D.5
  abs       = undefined
  fromInteger a = const' $ fromInteger a                        -- D.6



-- We make Expr an instance of Fractional so we can use the '/' operator.

instance (Show a, Integral a) => Fractional (Expr a) where               -- E.1
  a / b = frac a b                                                       -- E.2
  fromRational _ = error "Only integer constants are allowed in Expr."             -- E.3


-- We provide our own definition of the ^ function for exponentiation

(^) :: (Integral a, Show a) => Expr a -> Int -> Expr a                                  -- Q.1
a ^ p = exp_ a p                                                                -- Q.2


-- We make Expr an instance of Ord so that we can compare and sort expressions.

instance (Show a, Ord a, Integral a) => Ord (Expr a) where                  -- L.1

    compare (Const a) (Const b)               = compare a b
    compare (Neg (Const _)) (Const _)         = LT
    compare (Const _) (Neg (Const _))         = GT
    compare (Neg (Const a)) (Neg (Const b))   = compare b a

    compare (Neg a) (Neg b)       = compare a b                             -- L.3
    compare a (Neg b)
             | a == b    = GT
             | otherwise = compare a b
    compare (Neg a) b
             | a == b    = LT
             | otherwise = compare a b

    compare a b                                                             -- L.4
                | da < db       = LT
                | da > db       = GT
                | otherwise     = compare' a b
                    where
                        da = degree a
                        db = degree b


-- A function for comparing expressions with equal degree
compare' :: (Show a, Ord a, Integral a) => Expr a -> Expr a -> Ordering

compare' (Frac _ _) (Frac _ _) = undefined                                      -- L.5
compare' (Frac _ _) _   = GT
compare' _ (Frac _ _)   = LT

compare' (Symbol a) (Symbol b) = compare b a                                    -- L.6

compare' (Sum as) (Sum bs) = cList as bs                                        -- L.9
    where
        cList [] []         = EQ                                                -- L.10
        cList [] _          = GT                                                -- L.11
        cList _  []         = LT
        cList (m:ms) (n:ns) = mappend (compare m n) $ cList ms ns               -- L.12

compare' (Sum as) e = cmp as e                                                  -- L.13
    where
        cmp [] _     = LT
        cmp (n:ns) m = mappend (compare n m) $ cmp ns m

compare' e s@(Sum _) = flipCompare $ compare' s e                               -- L.14

compare' (Exp a pa) (Exp b pb)                                                  -- L.19
        | pa < pb       = LT
        | pa > pb       = GT
        | otherwise     = compare' a b

compare' (Prod as) (Prod bs) = cmpProdList as bs

compare' (Exp _ _) _ = GT                                                       -- L.8
compare' _ (Exp _ _) = LT

compare' e p@(Prod _) = compare' (Prod [e]) p
compare' p@(Prod _) e = compare' p (Prod [e])

compare' _ _ = error "compare should never end up here"


-- Compare two list of expressions from inside Prod
cmpProdList :: (Integral a, Show a) => [Expr a] -> [Expr a] -> Ordering
cmpProdList as bs = cList (reverse as) (reverse bs)                             -- L.15
    where
        cList [] []           = EQ                                              -- L.16
        cList [] _            = LT
        cList _  []           = GT
        cList (m:ms) (n:ns)   = mappend (cmp m n) (cList ms ns)                 -- L.17

        cmp a@(Symbol _) b@(Symbol _)           = compare' a b                  -- L.18

        cmp (Exp (Symbol a) pa) (Exp (Symbol b) pb)
                | a == b    = indexCompare pa pb
                | otherwise = compare b a

        cmp (Symbol a) (Exp (Symbol b) _)
                | a == b    = GT
                | otherwise = compare b a
        cmp e@(Exp (Symbol _) _) s = flipCompare $ cmp s e

        cmp a b = compare a b


-- Flip the provided Ordering
flipCompare :: Ordering -> Ordering
flipCompare LT = GT
flipCompare GT = LT
flipCompare EQ = EQ


-- Compute Ordering based on comparing index powers
indexCompare :: Int -> Int -> Ordering
indexCompare a b
    | d < 0     = GT
    | d > 0     = LT
    | otherwise = EQ
        where
            d = a - b



-- Calculate the degree of an expression (polynomial)
degree :: Num a => Expr a -> Int                                  -- O.1
degree (Const _)   = 0
degree (Symbol _)  = 1
degree (Neg e)     = degree e
degree (Frac n d)  = degree n - degree d
degree (Prod xs)   = sum $ map degree xs                          -- O.2
degree (Sum [])    = 0                                            -- O.3
degree (Sum xs)    = foldl1 max $ map degree xs                  -- O.4
degree (Exp e pwr) = pwr * degree e



-- We define functions that intelligently carry out the various arithematic operations.

-- Creating the reciprocal of an expression

--rec' :: Integral a => Expr a -> Expr a
--rec' (Rec e)    = e
--rec' e          = Rec e


-- Negation of an expression

neg' :: Integral a => Expr a -> Expr a
neg' (Const 0)  = Const 0
neg' (Neg e)    = e
neg' e          = Neg e


-- Adding expressions

sum_list :: Integral a => [Expr a] -> Expr a                            -- Z.1
sum_list []     = Const 0
sum_list [e]    = e
sum_list es     = Sum es


sum_ :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

sum_ (Const 0) e    = e                                                 -- Z.2
sum_ e (Const 0)    = e

sum_ (Neg a) (Neg b) = Neg (sum_ a b)                                   -- Z.3
sum_ a b@(Neg _)     = sum_ b a

sum_ (Neg a@(Sum as)) b
       | a == b     = Const 0                                           -- Z.4
       | otherwise  = sum_ (sum_list $ map neg' as) b

sum_ (Sum [e]) s2@(Sum _)   = sum_ e s2                                 -- Z.5
sum_ s1@(Sum (e:es)) s2@(Sum _)
        | s1 == s2          = 2 * s1
        | otherwise         = sum_ (sum_list es) (sum_ e s2)

sum_ a b@(Sum ss) = branch $ match a ss                                 -- Z.6
                        where
                            branch Nothing   = sum' a b
                            branch (Just es) = sum_list es

                            match _ []          = Nothing

                            match c@(Neg d) (e:es)
                                    | d == e    = Just es
                                    | otherwise = fmap (e:) (match c es)

                            match c (d@(Neg e):es)
                                    | c == e    = Just es
                                    | otherwise = fmap (d:) (match c es)

                            match c (e:es)
                                    | c == e    = Just $ (2 * c):es
                                    | otherwise = fmap (e:) (match c es)


sum_ sa@(Sum _) e       = sum_ e sa                                             -- Z.7

sum_ ea@(Neg a) b                                                               -- Z.8
        | a == b        = Const 0
        | otherwise     = sum' ea b

sum_ a b                                                                        -- Z.8
        | a == b        = 2 * a
        | otherwise     = sum' a b


sum' :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

sum' a@(Const _) b          = sum_c a b                 -- Z.9
sum' a@(Neg (Const _)) b    = sum_c a b

sum' a@(Prod _) b           = sum_p a b
sum' a@(Neg (Prod _)) b     = sum_p a b

sum' a b    = sum_x a b                                 -- Z.10


-- Rules for adding Const to other expressions

sum_c :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

sum_c (Const a) (Const b)               = Const (a + b)

sum_c (Neg (Const a)) (Const b)
        | c < 0     = Neg (Const $ negate c)
        | otherwise = Const c
            where
                c = b - a

sum_c a (Sum bs) = sum_list $ add a bs                                                                   -- AA.1
                        where
                            add (Const 0) es             = es
                            add e []                     = [e]                                           -- AA.2
                            add c (d@(Const _):es)       = add (c + d) es                                -- AA.3
                            add c (d@(Neg (Const _)):es) = add (c + d) es

                            add c (e:es) = case (compare c e) of                                         -- AA.4
                                                    GT -> c:e:es
                                                    LT -> e:(add c es)
                                                    EQ -> add (c + e) es

sum_c a b  = sum_c a (Sum [b])                                                                           -- AA.5


-- Rules for adding non-Const/Sum/Prod expressions with other expressions

sum_x :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

sum_x a b@(Const _) = sum_c b a                                                 -- AB.1

sum_x a (Sum bs) = sum_list $ add a bs                                          -- AB.2
                        where
                            add c []     = [c]
                            add c (d:es) = case (compare c d) of
                                                GT -> c:d:es
                                                LT -> d:(add c es)
                                                EQ -> (2 * c):es

sum_x a b = case (compare a b) of                                         -- AB.3
                    GT -> Sum [a, b]
                    LT -> Sum [b, a]
                    EQ -> Sum [a, b] -- error "Only equal expressions should have equal compare result. In that case execution shouldn't arrive here. -- ToDO: Replace with error when all additions have been exhaustedle


-- Rules for adding a Prod with other expressions

sum_p :: (Show a, Integral a) => Expr a -> Expr a -> Expr a                     -- AC.1

sum_p a b@(Prod _)       = sum_pc a b                                           -- AC.2
sum_p a b@(Neg (Prod _)) = sum_pc a b

sum_p a b                = sum_x a b                                            -- AC.3


sum_pc :: (Show a, Integral a) => Expr a -> Expr a -> Expr a
sum_pc pa pb = add (split pa) (split pb)
        where
            split (Prod ((Const c):es))         = (c, es)                       -- AC.4
            split (Neg (Prod ((Const c):es)))   = (-c, es)
            split (Prod es)                     = (1, es)
            split (Neg (Prod es))               = (-1, es)

            add (a, as) (b, bs)                                                 -- AC.5
                | as == bs  = fromIntegral d * Prod as
                | otherwise = sum_x (fromIntegral a * Prod as) (fromIntegral b * Prod bs)
                        where
                            d = a + b


-- Multiplying

prod_list :: Integral a => [Expr a] -> Expr a
prod_list []     = Const 1
prod_list [e]    = e
prod_list es     = Prod es


prod_ :: (Show a, Integral a) => Expr a -> Expr a -> Expr a               -- R.1a

prod_ (Neg a) (Neg b)   = prod_ a b                             -- R.1b
prod_ (Neg a) b         = neg' (prod_ a b)
prod_ a (Neg b)         = neg' (prod_ a b)

prod_ (Const 0) _       = Const 0                               -- R.1c
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
                                    | c == e      = Just $ (c^2):es
                                    | otherwise   = fmap (e:) (match c es)

prod_ p@(Prod _) e      = prod_ e p                     -- R.7

prod_ ea eb                                             -- R.7
        | ea == eb      = ea^2
        | otherwise     = prod' ea eb



prod' :: (Show a, Integral a) => Expr a -> Expr a -> Expr a       -- T.1

prod' c@(Const _) e     = prod_c c e                    -- T.2
prod' sym@(Symbol _) e  = prod_s sym e
prod' e@(Exp _ _) d     = prod_e e d
--prod' r@(Rec _) e       = prod_r r e
prod' sm@(Sum _) e      = prod_sm sm e

prod' a b = error $ "Patterns for multiplication exhausted. The patterns aren't comprehensive.: " ++ show a ++ " * " ++ show b



-- Rules for multiplying Const with other expressions
prod_c :: (Show a, Integral a) => Expr a -> Expr a -> Expr a                              -- U.1a

prod_c (Const a) (Const b)               = Const (a*b)                          -- U.1b
prod_c a@(Const _) sym@(Symbol _)        = Prod [a, sym]                        -- U.2
prod_c a@(Const _) b@(Exp _ _)           = Prod [a, b]                          -- U.3
--prod_c a@(Const _) b@(Rec _)             = Prod [b, a]                          -- U.4
prod_c a@(Const _) b@(Sum _)             = Prod [a, b]

prod_c c@(Const v) (Prod ps) = Prod $ mul v ps                                           -- U.5
                                where
                                    mul a ((Const b):es)        = Const (a * b):es       -- U.5a
--                                    mul a (r@(Rec _):es)        = r:(mul a es)           -- U.5b     -- ToDo: Implement cancellation
                                    mul _ es                    = c:es                   -- U.5c

prod_c a b = error $ "prod_c is only intended for multiplying Const with other expressions: " ++ show a ++ " * " ++ show b           -- U.6


-- Rules for multiplying Symbols with other expressions
prod_s :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

prod_s sa@(Symbol _) c@(Const _)             = prod_c c sa                       -- V.1

prod_s sa@(Symbol a) sb@(Symbol b)                                               -- V.2
                                | a < b      = Prod [sa, sb]
                                | otherwise  = Prod [sb, sa]

prod_s sa@(Symbol a) eb@(Exp (Symbol b) _)                                       -- V.3
                                        | a < b      = Prod [sa, eb]
                                        | otherwise  = Prod [eb, sa]

prod_s sa@(Symbol _) e@(Exp _ _ )    = Prod [sa, e]                              -- V.4
--prod_s sa@(Symbol _) r@(Rec _)       = Prod [r, sa]                              -- ToDo: Deal with cancellation
prod_s sa@(Symbol _) ss@(Sum _)      = Prod [sa, ss]

prod_s sa@(Symbol a) (Prod ps)       = Prod $ mul a ps                                                            -- V.5
                                            where
                                                mul b (c@(Const _):es)      = c:(mul b es)                        -- V.6
                                                -- ToDo: Add functionality for cancellation of symbol with Rec
--                                                mul _ (r@(Rec _):es)        = r:(mul a es)

                                                mul b (sc@(Symbol c):es)                                          -- V.7
                                                                | b < c      = sa:sc:es
                                                                | otherwise  = sc:(mul b es)

                                                mul b (e@(Exp (Symbol c) _):es)                                   -- V.8
                                                                | b < c      = sa:e:es
                                                                | otherwise  = e:(mul b es)

                                                mul _ es                     = sa:es                              -- V.9

prod_s a b = error $ "prod_s is only intended for multiplying Symbol expression: " ++ show a ++ " * " ++ show b


-- Rules for multiplying Exp with other expressions
prod_e :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

prod_e ea@(Exp _ _) c@(Const _)     = prod_c c ea                                    -- W.1
prod_e ea@(Exp _ _) sb@(Symbol _)   = prod_s sb ea

prod_e ea@(Exp (Symbol a) _) eb@(Exp (Symbol b) _)                                   -- W.2
                                | a < b     = Prod [ea, eb]
                                | otherwise = Prod [eb, ea]

prod_e ea@(Exp (Symbol _) _) eb@(Exp _ _)   = Prod [ea, eb]                          -- W.3
prod_e ea@(Exp _ _) eb@(Exp (Symbol _) _)   = Prod [eb, ea]

--prod_e ea@(Exp _ _) r@(Rec _)               = Prod [r, ea]
prod_e ea@(Exp (Symbol _) _) sm@(Sum _)     = Prod [ea, sm]                          -- W.4

prod_e ea@(Exp _ _) sm@(Sum _)     = Prod [sm, ea]                                   -- W.5

prod_e ea@(Exp (Symbol a) n) (Prod ps)   = Prod $ mul a n ps                                       -- W.6
                                            where
                                                mul b p (c@(Const _):es)    = c:(mul b p es)
--                                                mul b p (r@(Rec _):es)      = r:(mul b p es)

                                                mul b p (sc@(Symbol c):es)
                                                                | b < c     = ea:sc:es
                                                                | b > c     = sc:(mul b p es)
                                                                | otherwise = (exp' (Symbol b) (p+1)):es

                                                mul b p (ec@(Exp (Symbol c) pc):es)
                                                                | b < c     = ea:ec:es
                                                                | b > c     = ec:(mul b p es)
                                                                | otherwise = (exp' (Symbol b) (p + pc)):es

                                                mul _ _ es              = ea:es

prod_e ea@(Exp a n) (Prod ps)   = Prod $ mul a n ps                                                -- W.7
                                        where
                                            mul _ _ []                  = [ea]
                                            mul b p (c@(Const _):es)    = c:(mul b p es)
--                                            mul b p (r@(Rec _):es)      = r:(mul b p es)

                                            mul b p (ec@(Exp c pc):es)
                                                        | b == c        = (exp' b (p + pc)):es
                                                        | otherwise     = ec:(mul b p es)

                                            mul b p (e:es)
                                                        | b == e        = (exp' b (p + 1)):es
                                                        | otherwise     = e:(mul b p es)

prod_e a b = error $ "prod_e is only intended to multiply by Exp: " ++ show a ++ " * " ++ show b


-- Rules for multiplyng Rec with other expressions
--prod_r :: (Show a, Integral a) => Expr a -> Expr a -> Expr a
--
--prod_r r@(Rec _) c@(Const _)              = prod_c c r                              -- X.1
--prod_r r@(Rec _) sym@(Symbol _)           = prod_s sym r
--prod_r r@(Rec _) e@(Exp _ _)              = prod_e e r
--
--prod_r (Rec ra) (Rec rb)                  = rec' (prod_ ra rb)            -- ToDo: Implement cancellation.
--prod_r r@(Rec _) sm@(Sum _)               = Prod [r, sm]
--prod_r rc@(Rec _) (Prod (rp@(Rec _):es))  = mul $ (prod_ rc rp):es                  -- X.2
--                                                where
--                                                    mul [e]    = e
--                                                    mul (a:as) = prod_ a (mul as)
--
--prod_r rc@(Rec _) (Prod es)               = Prod $ rc:es                            -- X.3
--
--prod_r a b  = error $ "prod_r can only be used to multiply Rec: " ++ show a ++ " * " ++ show b



-- Rules for multiplying Sum with other expressions
prod_sm :: (Show a, Integral a) => Expr a -> Expr a -> Expr a

prod_sm sm@(Sum _) c@(Const _)      = prod_c c sm
prod_sm sm@(Sum _) sym@(Symbol _)   = prod_s sym sm
prod_sm sm@(Sum _) e@(Exp _ _)      = prod_e e sm
--prod_sm sm@(Sum _) r@(Rec _)        = prod_r r sm

prod_sm sa@(Sum _) sb@(Sum _) = case cmp_prod sa sb of                   -- Y.1
                                    GT -> Prod [sa, sb]
                                    LT -> Prod [sb, sa]
                                    EQ -> error "Equal expressions shouldn't appear in prod_sm"

prod_sm sa@(Sum _) (Prod ps) = prod_list $ mul sa ps                                            -- Y.2
                                  where
                                      mul a []      = [a]                                       -- Y.3
                                      mul a (e@(Symbol _):es) = e:(mul a es)                    -- Y.4
                                      mul a (e@(Exp (Symbol _) _):es)  = e:(mul a es)

                                      mul a (e:es)  = case cmp_prod a e of                      -- Y.5
                                                            GT -> a:e:es
                                                            LT -> e:(mul a es)
                                                            EQ -> error "Equality should already have been tested for"


cmp_prod :: (Show a, Integral a) => Expr a -> Expr a -> Ordering
cmp_prod a b
    | degree a < degree b   = GT                    -- Y.6
    | degree a > degree b   = LT
    | otherwise             = compare a b


-- Exponentiation of expressions

exp_ :: (Integral a, Show a) => Expr a -> Int -> Expr a
exp_ e p                                                        -- S.1
    | p > 0     = exp' e p
    | p < 0     = frac (Const 1) (exp' e $ abs p)
    | otherwise = Const 1


exp':: Integral a => Expr a -> Int -> Expr a

exp' (Neg e) p                                                  -- S.2
        | odd p     = neg' (exp' e p)
        | otherwise = exp' e p

--exp' (Rec e) p      = rec' (exp' e p)                            -- S.3

exp' _ 0 = Const 1
exp' e 1 = e

exp' (Const c) p     = Const (c Prelude.^ p)                    -- S.4
exp' (Exp e p) q     = Exp e (p * q)
exp' e p             = Exp e p                                  -- S.5


frac :: (Integral a, Show a) => Expr a -> Expr a -> Expr a
frac a b
    | a == b        = Const 1                                   -- AD.1
    | otherwise     = frac' a b


frac' :: (Integral a, Show a) => Expr a -> Expr a -> Expr a

-- ToDo: Implement division where one or both expressions are of type Frac
-- ToDo: Implement Prod / Prod
frac' (Prod as) (Prod bs) = undefined

frac' (Prod as) b = divide [] as b                              -- AD.2
                        where
                            divide es [] d       = Frac (Prod (reverse es)) d               -- AD.3
                            divide es (x:xs) d   = branch es xs x d $ frac_ x d             -- AD.4

                            branch es xs x d Nothing    = divide (x:es) xs d
                            branch es xs _ _ (Just e)   = prod_ e $ prod_list ((reverse es) ++ xs)

frac' a p@(Prod _)  = rec $ frac' p a                           -- AD.5


frac' a b = branch $ frac_ a b                                  -- AD.6
                where
                    branch Nothing  = Frac a b
                    branch (Just e) = e


frac_ :: (Integral a, Show a) => Expr a -> Expr a -> Maybe (Expr a)           -- AD.7
frac_ (Exp a p) (Exp b q)
    | a == b        = Just $ exp_ a (p - q)
    | otherwise     = Nothing

frac_ (Exp a p) b
    | a == b        = Just $ exp_ a (p - 1)
    | otherwise     = Nothing

frac_ a (Exp b p)
    | a == b        = Just $ Frac (Const 1) (exp_ b (p - 1))
    | otherwise     = Nothing

frac_ a b
    | a == b        = Just $ Const 1
    | otherwise     = Nothing


rec :: (Integral a, Show a) => Expr a -> Expr a
rec (Frac n d) = Frac d n
rec e = Frac 1 e


-- Define a debug function for tracing code
debug :: String -> a -> a
debug m a = trace ("\nDEBUG: " ++ m) a

-- Define a debug function for printing values
debug_vars :: Show a => [(String, a)] -> b -> b
debug_vars es b = debug (prnt es) b
    where
        prnt []            = error "debug_vars called with empty list"
        prnt [(s, e)]      = s ++ ": " ++ show e
        prnt ((s, e):xs)   = s ++ ": " ++ show e ++ " _ " ++ prnt xs