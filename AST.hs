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

data Circuit = C { c_inputs  :: [Ident]        -- inputs 
                  , c_outputs :: [Ident]        -- outputs 
                  , c_eqs     :: [Equation]     -- equations 
                  }
             deriving (Show)

-- TODO : Perhaps that I need to sort the eqs sortedInput
-- sortedOutputs and ten sortedIntemediates
-- I can do this when I build the FullCircuit
-- TODO: Think about composition
-- So fc_eqs contains eqs
data FullCircuit = Fc { fc_inputs  :: [Ident]        -- inputs 
                  , fc_outputs :: [Ident]        -- outputs 
                  , fc_intermediate :: [Ident]   -- intermediate var 
                  , fc_eqs     :: [Equation]     -- equations 
                  }
             deriving (Show)




