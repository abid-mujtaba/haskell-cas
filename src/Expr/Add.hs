-- | Implement the addition arithmetic operation for expressions (with all of its inherent complexity).
module Expr.Add
    (
        add
    )
    where

import Expr (Expr(Atom, Add))

-- The addition operation is implemented using this top-level function which takes two expressions and returns their sum
-- Every Add expression is encapsulated inside an Atom for ease of similarity checks down the line
add :: Expr -> Expr -> Expr
add x (Atom 0 _ _) = x              -- Implements additive identity. Any expression plus zero is unchanged. Note the pattern match using the coeff (first element) of Atom alone which if it is zero corresponds to the constant zero regardless of the core or power
add (Atom 0 _ _) x = x
add x y = Atom 1 (Add x y) 1        -- Naive implementation of addition. Every "returned" (fully processed) expression in this CAS "MUST" be encapsulated as an Atom so that the initial patterns designed around this assumption can come in to play
