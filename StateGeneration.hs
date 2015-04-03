{-# LANGUAGE TupleSections#-}

module StateGeneration where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import qualified Data.List as List
import Data.Vector
import Data.Functor
import qualified Data.Map as Map

import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Checking

type Vertex = Vector Bool --Of size fixed 

data StateGraph =
	SG { n_v :: Integer,
		edges :: Map.Map Vertex [([Integer],[Integer])]			
	}		
	deriving (Show)

-- Generate all the states with list and then convert one time to Vectors
generateListVertexList :: Integer -> [[Bool]]
generateListVertexList 0 = [ [True], [False] ]
generateListVertexList n = let prev = generateListVertexList (n-1) in
					(List.map (\x -> False:x) prev) List.++ (List.map (\x -> True:x) prev)
	
generateListVertexVector :: Integer -> [ Vector Bool ]
generateListVertexVector n = List.map fromList $ generateListVertexList n

generateEmptyGraph n = SG n . Map.fromList . List.map (\x -> (x,[])) $ generateListVertexVector n

computeTransitionByCircuit :: FullCircuit -> StateGraph
computeTransitionByCircuit = undefined




