{-# LANGUAGE TupleSections#-}

module Logic where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many, empty)

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
import Data.Graph.Inductive.PatriciaTree




subIsomorphisms g1 g2 = 
	foldr 
	(\f acc -> \l x -> f l x >>= acc (x:l))
	(\l x -> guard (isIso g2 $ removeLast (x:l)) >> return (fmap (fromJust.lab g2)  $ x:l))
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

type Isomorphisms = [Map.Map Term Term]

satifyF :: Isomorphisms -> INF -> Formula Input
satifyF i [] = tt
satifyF i ((IClause a b):q) = FOL.and (FOL.or ( FOL.not $ satifyCN i a ) $ satifyCP i b) $ satifyF i q 

satifyCP :: Isomorphisms -> [Atom ()] -> Formula Input
satifyCP i [] = ff
satifyCP i (t:q) = FOL.or (substitue i t) $ satifyCP i q 

satifyCN :: Isomorphisms -> [Atom ()] -> Formula Input
satifyCN i [] = tt
satifyCN i (t:q) = FOL.and (substitue i t) $ satifyCN i q

substitue :: Isomorphisms -> Atom () -> Formula Input 
substitue i (Atom s l) = foldl 
			(\acc iso -> acc `FOL.or` if s == "E" 
					then atom s (fmap (\x -> iso Map.! x) l) 
					else atom s l )
				ff
				i


printSatFormulas formula stateGraph = 
	satifyF isomorphisms normalized
	where	normalized = normalize formula
		isomorphisms =fmap (\x -> Map.fromList . zip 
						( fmap 	(FOL.Var . fromJust.lab pattern) 
							$ nodes pattern)
						. fmap Var $ x)
				 $ subIsomorphisms pattern stateGraph [] 0
		pattern = extractPattern normalized	
--
--instance Graph StateGraph where
--	empty = 
--	isEmpty = 
--	match = 
--	mkGraph =
--	labNodes = 

extractPattern :: INF -> Gr String String    		
extractPattern [] = empty  	
extractPattern ((IClause a _):_) = inductivelyBuild a
	where 	inductivelyBuild [] = empty
		inductivelyBuild (Atom s ln:q) = 
			let subGraph = inductivelyBuild q in
			let l = newNodes 2 subGraph in
			let withNode =	insNodes 
						(zip l .
						 take 2 .
						 fmap (\x->case x of
								Var s -> s
								_ -> undefined)
						 $ ln )
						subGraph in
			insEdges ([])
				withNode

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


