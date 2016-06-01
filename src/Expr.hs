module Expr
    where

data Expr a =                               -- B.1, B.2
              Const a                       -- B.3
            | Sum [Expr a]                  -- B.4
            | Prod [Expr a]
            | Neg (Expr a)
            | Frac (Expr a) (Expr a)        -- B.5
            | Exp (Expr a) Int              -- B.6
            | Symbol String                 -- B.7
            deriving (Eq)                   -- B.8
