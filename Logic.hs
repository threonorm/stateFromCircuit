{-# LANGUAGE TupleSections#-}

--module Logic where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many, empty)

import Data.Char
import Data.List 
import qualified Data.Vector as Vect 
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

--Come from the lib
nodeComp :: Eq b => LNode b -> LNode b -> Ordering
nodeComp n@(v,_) n'@(w,_) | n == n'   = EQ
                          | v<w       = LT
                          | otherwise = GT

slabNodes :: (Eq a,Graph gr) => gr a b -> [LNode a]
slabNodes = sortBy nodeComp . labNodes

edgeComp :: Eq b => LEdge b -> LEdge b -> Ordering
edgeComp e@(v,w,_) e'@(x,y,_) | e == e'              = EQ
                              | v<x || (v==x && w<y) = LT
                              | otherwise            = GT

slabEdges :: (Eq b,Graph gr) => gr a b -> [LEdge b]
slabEdges = sortBy edgeComp . labEdges

included:: (Eq a,Eq b,Graph gr) => gr a b -> gr a b -> Bool
included g g' = slabNodes g == slabNodes g' && intersect (slabEdges g) (slabEdges g') == slabEdges g





subIsomorphisms g1 g2 = 
	foldr 
	(\f acc -> \l x -> f l x >>= acc (x:l))
	(\l x -> guard (isIso g2 $ removeLast (x:l)) >> return (fmap (fromJust.lab g2) . removeLast $ x:l))
	( replicate (noNodes g1) $ \x y -> nodes g2 \\ (y:x) ) --):(replicate (noNodes g1 -1) (\x y -> neighbors g2 y \\ x) )) -- neighbor g2)
	where 
		isIso g l =included g1' $ mkGraph --everything is here
				(fmap (\x -> (renameBy x l,())) l)
				(fmap (\(x,y,z) -> (renameBy x l,renameBy y l ,())) 
				. catMaybes . fmap (extractEdges l) 
				$ labEdges g) 
		renameBy x = (+1) . fromJust . elemIndex x
		g1' = emap (\x->()) $ nmap (\x -> ()) g1
		extractEdges l (x,y,z) = if elem x l && elem y l then Just (x,y,z) else Nothing 
		removeLast x = take (length x - 1) x

type Isomorphism = Map.Map Term Term
type Isomorphisms = [Isomorphism]

satifyF :: Isomorphisms -> INF -> INF
satifyF iso clause =
	foldl (\acc i->(++) acc $ fmap 
					(\(IClause a b) -> IClause (satifyCN i a) (satifyCP i b))
					 clause )
		[]	
		iso 

satifyCP :: Isomorphism -> [Atom ()] -> [Atom ()]
satifyCP i l =simplify . fmap (substitue i)$ l

satifyCN :: Isomorphism -> [Atom ()] -> [Atom ()]
satifyCN i l= simplify. fmap (substitue i) $l

substitue :: Isomorphism -> Atom () -> Atom ()
substitue i (Atom s l) =
	Atom s ((fmap (\x -> i Map.! x) $ take 2 l)++drop 2 l) 


simplify :: [ Atom () ] -> [ Atom () ]
simplify x =fromMaybe [] . foldl 
		(\acc (Atom s l) -> case acc of
					Nothing -> Nothing
					Just j -> if s=="Eq" 
						then	if l!!0 == l!!1 
								then Nothing
								else Just j
			 			else Just $ (Atom s l):j) 
		(Just [])
		$ x	
		

printSatFormulas formula stateGraph = 
	satifyF isomorphisms normalized
	where	normalized = normalize formula
		isomorphisms =fmap (\x -> Map.fromList . zip 
						( fmap 	(FOL.Var . fromJust.lab pattern) 
							$ nodes pattern)
						. fmap Var $ x)
				 $ subIsomorphisms pattern stateGraph [] 0
		pattern = extractPattern normalized	

intF [] = 0
intF (t:q) = if t then 2* intF q +1 else 2 * intF q 

convertGraph :: StateGraph -> Gr String String
convertGraph sg = --It is not fully built by lazyness,
	mkGraph v e  --BIG IMPROVEMENT HERE TODO
	where 	(v,e) = Map.foldWithKey
			(\k (pos,neg) (accV,accE) -> 
				let key = Vect.toList k in 
				(if pos == [] && neg ==[]
					then accV 
					else (intF key+1, binary key):accV
				,fmap (\x -> (intF key+1, intF key+2^x +1, show x ++ "+")) pos ++
				 fmap (\x -> (intF key+1, intF key-2^x + 1, show x ++ "-")) neg ++
				 accE)
			)
			([],[])
			$ StateGeneration.edges sg  
		binary = fmap (\f->if f then '1' else '0')	


extractPattern :: INF -> Gr String String    		
extractPattern [] = empty  	
extractPattern arg1@((IClause a q):l) = addVertex (extractVertex arg1) $ inductivelyBuild a
	where 	inductivelyBuild [] = empty
		inductivelyBuild (Atom s ln:q) = 
			let subGraph = inductivelyBuild q in
			let l = newNodes 2 subGraph in
			let withNode =	insNodes 
						(zip l .
						 filter (\x-> Prelude.not . elem x .fmap (\(a,b)-> b) $ labNodes subGraph) .take 2. fmap (\x->case x of
								Var s -> s
								_ -> undefined)
							 $ ln )
						subGraph in
			if s=="E" 
				then insEdge 	(fromJust. lookup (case ln!!0 of
									Var s -> s
									_ -> undefined) . fmap (\(x,y)->(y,x)) $ labNodes withNode ,
						fromJust. lookup (case ln!!1 of
									Var s -> s
									_ -> undefined) . fmap (\(x,y)->(y,x)) $ labNodes withNode ,
						 case (ln!!2) of 
							Var s -> s
							_ -> undefined)
						withNode
				else withNode

giveName x = case x of
		Atom s l -> fmap (\m -> case m of Var s -> s) $ take 2 l 

extractVertex :: INF -> [String]
extractVertex arg1 = concat . concat .  fmap (\x -> case x of
				IClause a b -> (fmap giveName a) ++ fmap giveName b)  $ arg1

addVertex [] g = g
addVertex (t:q) g =if elem t . fmap snd $ labNodes g
			then addVertex q g
			else addVertex q (insNode (head $ newNodes 1 g,t) g)

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


main =do
	myLine <- getLine
	result <-parseFromFile netlistParser myLine
	case result  of
		Left a -> putStrLn "fail"
		Right b -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $b in	
					do 	putStrLn .show . convertGraph $ sg
						putStrLn . show . extractPattern . normalize $ outputPersistency
						putStrLn . show . pretty . printSatFormulas outputPersistency $ convertGraph sg


