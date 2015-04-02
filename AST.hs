module AST where

import qualified Data.Map as Map

type Ident = String
type Environment t = Map.Map Ident t


data Binop = Or | Xor | And | Nand
           deriving (Show)

binopFn Or = (||)
binopFn Xor = \p q -> (p || q) && not (p && q)
binopFn And = (&&)
binopFn Nand = \p q -> not $ p && q

-- argument of operators (variable or constant) 

data Exp = Earg Ident -- a: Argument 
         | Enot Exp -- NOT a 
         | Ebinop Binop Exp Exp -- OP a1 a2 : boolean operator 
         deriving (Show)

-- equations: x = exp 
type Equation = (Ident, Exp)

data Circuit = Pr { c_inputs  :: [Ident]        -- inputs 
                  , c_outputs :: [Ident]        -- outputs 
                  , c_eqs     :: [Equation]     -- equations 
                  }
             deriving (Show)




