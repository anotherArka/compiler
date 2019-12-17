module Expressions where

import Numeric.Natural

data Expression = 
      Constant Natural 
    | Var String 
