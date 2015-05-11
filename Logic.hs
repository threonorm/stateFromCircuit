{-# LANGUAGE TupleSections#-}

module Logic where

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
import Data.Tuple (swap)
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
	(\f acc -> \l -> f l >>= acc)
	(\l -> guard (isIso g2 $ l) >> return (fmap (fromJust.lab g2) $ l))
	. reverse . snd . foldl 	 
		(\(prev,sol) new-> (\x -> (new:prev,x)) . (:sol) $ (\x -> do
				 	y <- choice g2 prev new 0 x \\ x 
					return $ y:x)  
		)
		([],[])
		 $ nodes g1--Here I need an improvment 
	where
		choice g2 [] new n = (\x -> nodes g2) --Eta expansion not needed, test of style
		choice g2 (t:q) new n = (\x -> if elem new $ pre g1 t
					then pre g2 . (x!!) $ n --not sure
					else if elem new $ suc g1 t 
						then suc g2 . (x!!) $ n 
						else choice g2 q new (1+n) x)
		isIso g l =included g1' $ mkGraph --everything is here
				(fmap (\x -> (renameBy x l,())) l)
				(fmap (\(x,y,z) -> (renameBy x l,renameBy y l ,())) 
				. catMaybes . fmap (extractEdges l) 
				$ labEdges g) 
		renameBy x = (+1) . fromJust . elemIndex x
		g1' = emap (\x->()) $ nmap (\x -> ()) g1
		extractEdges l (x,y,z) = if elem x l && elem y l then Just (x,y,z) else Nothing 

type Isomorphism = Map.Map Term Term
type Isomorphisms = [Isomorphism]

--satifyF :: Isomorphisms -> INF -> INF
satifyF g iso clause =
	foldl (\acc i->(++) acc $ fmap 
					(\(IClause a b) -> IClause (satifyCN g i a) (satifyCP g i b))
					 clause )
		[]	
		iso 

satifyCP g i l =simplify g. fmap (substitue i)$ l

satifyCN g i l= simplify g. fmap (substitue i) $l

substitue :: Isomorphism -> Atom () -> Atom ()
substitue i (Atom s l) =
	Atom s ((fmap (\x -> i Map.! x) $ take 2 l)++drop 2 l) 


--simplify :: [ Atom () ] -> [ Atom () ]
--This is not ocrrect because of the Eq proposition TODO
simplify g x =fromMaybe [] . foldl 
		(\acc (Atom s l) -> case acc of
					Nothing -> Nothing
					Just j -> if s=="Eq" 
						then	if l!!0 == l!!1 
								then Nothing
								else Just j
			 			else if isIn l g 
							then Just $ (Atom s l):j
							else Just j) 
		(Just [])
		$ x	
	

isIn l g = elem secondVertice $ suc g firstVertice 
	where 	noeuds = catMaybes . fmap (\(x,y)-> if (Var x) `elem` l 
			then Just (Var x,y)
			else Nothing )
			$ fmap swap dic
		dic = labNodes g
		firstVertice= fromJust . lookup (l!!0) $ noeuds
		secondVertice = fromJust. lookup (l!!1) $ noeuds

allIsomorphisms formula stateGraph = 
	 [isomorphisms pattern | pattern <- patterns]		
	where	isomorphisms pattern = fmap (\x -> Map.fromList . zip 
						( fmap 	(FOL.Var . fromJust.lab pattern) 
							$ nodes pattern)
						. fmap Var $ x)
				 $ subIsomorphisms pattern stateGraph []
		patterns = extractPattern formula 

printSatFormulas [] stateGraph = []
printSatFormulas normalized@(t:q) stateGraph = 
	satifyF stateGraph (concat $ allIsomorphisms normalized stateGraph) normalized
	++ ( printSatFormulas q stateGraph)

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


extractPattern :: INF -> [Gr String String]    		
extractPattern [] = [empty] -- NOW I BELIEVE this addVertex is bullshit	
extractPattern arg1@((IClause a q):l) = addVertex q (nub $ extractVertex arg1) $ inductivelyBuild a
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
							"")
						withNode
				else withNode


--USELESS
giveName x = case x of
		Atom s l -> fmap (\m -> case m of Var s -> s) $ take 2 l 

extractVertex :: INF -> [String]
extractVertex arg1 = concat . concat .  fmap (\x -> case x of
				IClause a b -> (fmap giveName a) ++ fmap giveName b)  $ arg1


vertexOf g l =fromJust . lookup l .fmap swap  $ labNodes g   

possibleEdges g [] t = []
possibleEdges g ((Atom s l):q) t =
	if s=="E" then 
	if ((t ==).(\m -> case m of Var s -> s) $ l!!0) || ((t==).(\m -> case m of Var s -> s)$ l!!1)
		then ((\m -> case m of Var s -> s) $ l!!0,(\m -> case m of Var s -> s) $ l!!1):possibleEdges g q t  
		else possibleEdges g q t 
	else possibleEdges g q t

-- The followign function was a good exercice but useless and actually false for the purpose. 
-- It is dead code for the outputPersistency.
addVertex b [] g = return g
addVertex c (t:q) g = (\x-> if x==[] then addVertex c q g else x) $
		do
			(a,b) <-possibleEdges g c $t --Dangerous : it build only positive instances 
			if (Prelude.not . elem t . fmap snd $ labNodes g)&&(( elem a . fmap snd $ labNodes g) || (elem b . fmap snd $ labNodes g)) 
				then let ng = insNode (head $ newNodes 1 g,t) g in
					 addVertex c q (insEdge (vertexOf ng $ a, vertexOf ng $ b,"") $ ng)
				else addVertex c q g

outputPersistency :: Formula Input
outputPersistency = forall $ \sommet -> 
	(forall $ \voisin1 -> (forall $ \voisin2 ->
	atom "E" [sommet,voisin1] `impl`
	(atom "E" [sommet,voisin2] `impl`
	((FOL.not $ atom "Eq" [voisin1,voisin2]) `impl`
	((forall $ \completeDiagram -> 
	atom "E" [voisin1,completeDiagram] `impl`
	atom "E" [voisin2,completeDiagram]))))))

--For Minisat+
variablesSat :: Gr String String -> String
variablesSat sg = (++";") . ("min: -1*" ++) . intercalate " -1*" . fmap (\(x,y)->
					(\(a,b)->"E"++a++b).(\(a,b)->(fromJust a, fromJust b)) $(lab sg x ,lab sg y))
				$ Data.Graph.Inductive.Graph.edges sg



