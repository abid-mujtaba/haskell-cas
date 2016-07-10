module CasPrelude
    (
          module Prelude
        , (^)
    )
    where

import qualified Prelude as P
import Prelude hiding ((^))

x ^ y = undefined
