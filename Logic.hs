{-# LANGUAGE TupleSections#-}

module Logic where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)

import Data.Char
import Data.List 
import Data.Maybe
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Debug.Trace as D

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
import Data.Graph.Inductive.Example
--instance Graph StateGraph where
--	empty = 
--	isEmpty = 
--	match = 
--	mkGraph =
--	labNodes = 



subIsomorphisms g1 g2 = 
	foldr 
	(\f acc -> \l x -> f l x >>= acc (x:l))
	(\l x -> guard (isIso g2 $ removeLast (x:l)) >> return (x:l))
	( replicate (noNodes g1) $ \x y -> nodes g2 \\ (y:x) ) --):(replicate (noNodes g1 -1) (\x y -> neighbors g2 y \\ x) )) -- neighbor g2)
	where 
		isIso g l =equal g1' $ mkGraph
				(fmap (\x -> (renameBy x l,())) l)
				(fmap (\(x,y,z) -> (renameBy x l,renameBy y l ,z)) 
				. catMaybes . fmap (extractEdges l) 
				$ labEdges g) 
		renameBy x = (+1) . fromJust . elemIndex x
		g1' = nmap (\x -> ()) g1
		extractEdges l (x,y,z) = if elem x l && elem y l then Just (x,y,z) else Nothing 
		removeLast x = take (length x - 1) x


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


