{-# LANGUAGE TupleSections#-}

module StateGeneration where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Data.Vector
import Data.Functor
import qualified Data.Map as Map

import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Checking

data Vertex = Vector Bool --Of size fixed 

data StateGraph =
	SG { n_v :: Integer,
		edges :: Map.Map Vertex [([Integer],[Integer])]			
	}		


-- Generate all the states with list and then convert one time to Vectors
generateListVertexList 0 = [ [True], [False] ]
generateListVertexList n = let prev = generateListVertexList (n-1) in
					(Data.List.map (\x -> False:x) prev) Data.List.++ (Data.List.map (\x -> True:x) prev)
	


generateListVertexVector n = fromList $ generateListVertexList n




computeTransitionByCircuit :: FullCircuit -> StateGraph
computeTransitionByCircuit = undefined


-- I need to find a good representation 

 
