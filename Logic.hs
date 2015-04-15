{-# LANGUAGE TupleSections#-}

module Logic where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)

import Data.Char
import qualified Data.List as List
import Data.Maybe
import Data.Vector hiding ((++))
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Checking
import Parser
import FOL
import StateGeneration

import Data.Graph.Inductive.Graph

--instance Graph StateGraph where
--	empty = 
--	isEmpty = 
--	match = 
--	mkGraph =
--	labNodes = 



--custom subgraph isomorphism algorithm
--First isIso is naive and doesn't use what we could put in the monad.
subIsomorphisms g1 g2 = 
	List.foldl 
	(\acc f -> \l x -> f l x >>= acc (x:l))
	(\l x -> guard (isIso g2 l) >> return l)
	( repeat $ neighbor g1 g2)
	where 
		isIso g l = equal g1 $ mkGraph (extractNodes $ labNodes g) (extractEdges $ labEdges g) 
		extractNodes = id
		extractEdges = id
		neighbor = undefined




-- I will need to declare some predicates for Edges, Neighbor, Edges  

outputPersistency :: Formula Input
outputPersistency = forall $ \sommet -> 
	(forall $ \voisin1 -> (forall $ \voisin2 ->
	(forall $ \transac1 -> (forall $ \transac2 ->
	atom "E" [sommet,voisin1,transac1] `impl`
	atom "E" [sommet,voisin2,transac2] `impl`
	(FOL.not $ atom "Eq" [voisin1,voisin2]) `impl`
	(forall $ \completeDiagram -> 
	atom "E" [voisin1,completeDiagram,transac2] `impl`
	atom "E" [voisin2,completeDiagram,transac1])))))


