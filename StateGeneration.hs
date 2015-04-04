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

-- inputs | outputs mixed with intermediates so n_v= n_int + n_inp + n_outpu
data StateGraph =
	SG { n_v :: Integer,
		n_inputs :: Integer,
		n_outputs :: Integer,
		edges :: Map.Map Vertex [([Integer],[Integer])]			
	}		
	deriving (Show)

-- Generate the 2^n states with list and then convert to Vectors to
-- gain time

generateListVertexList :: Integer -> [[Bool]]
generateListVertexList 0 = [ [True], [False] ]
generateListVertexList n = let prev = generateListVertexList (n-1) in
					(List.map (\x -> False:x) prev) List.++ (List.map (\x -> True:x) prev)
	
generateListVertexVector :: Integer -> [ Vector Bool ]
generateListVertexVector n = List.map fromList $ generateListVertexList n

-- Generate a graph with all the vertex needed, and then we will add the edges

generateEmptyGraph n i o= SG n i o. Map.fromList . List.map (\x -> (x,[])) $ generateListVertexVector n

computeTransitionByCircuit :: FullCircuit -> StateGraph
computeTransitionByCircuit fc =
	List.foldl 
		(\acc x ->
			addInputsChanges 
				x
				List.foldl
					(-- Probably a cast needed  
					if (x ! fromInteger i) /= evaluate fc x i  
						then changeS i x acc 
						else acc
					)
					acc
					[i| i <- [0..((List.length $fc_eqs fc)-1)]]
		)
		emptyGraph
		(Map.keys . edges $ emptyGraph ) 
	where 	emptyGraph = generateEmptyGraph  (v + i + o) i  o   
		i = toInteger . List.length $ fc_inputs fc
		o = toInteger . List.length $ fc_outputs fc
		v = toInteger . List.length $ fc_intermediate fc
		addInputsChanges v sg = undefined 
		evaluate fc x i = undefined
		changeS i x acc = undefined




