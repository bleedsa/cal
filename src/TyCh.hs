module TyCh where

import Data.Text

import Com
import Parse

data Expr = I Type Int  -- integer
          | X Type Text -- identifier
          | Let 
