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
add x y = Atom 1 (Add x y) 1
