{-# LANGUAGE TupleSections#-}


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
import FOL hiding (out)
import qualified StateGeneration as SG
import Logic

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree
import GHC.Exts (sortWith)

import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Text.Lazy (unpack)
import System.Environment
import Control.Arrow(first)
import Data.Function(on)
sign x y =  (\a b ->if x!!b == '1' then "-" else "+" ) x . fromJust . findIndex (\(x,y)-> x/=y ) $ x `zip` y

outputCircuitGraph circuit graph =
	".inputs " ++ concat (intersperse " " (fc_inputs circuit)) ++   
	"\n.outputs " ++ concat (intersperse " " (filter (\x-> take 3 x /= "csc" ) $ fc_outputs circuit)) ++
	"\n.internal " ++ concat (intersperse " " (fc_intermediate circuit ++ (filter (\x-> take 3 x == "csc") . fc_outputs $ circuit) )) ++
	"\n.state graph\n" ++ concat (fmap (\(x,y,l) -> (("S" ++)  . fromJust . lab graph $ x) ++ " " ++ l ++ sign (fromJust . lab graph $ x) (fromJust . lab graph $ y) 
					++( (" S" ++) . fromJust.lab graph $ y) ++ "\n"  ) $ labEdges graph ) ++
--	".marking {S" ++"}\n.end"
	".marking {S" ++ stableV graph circuit ++ "}\n.end"
	where   nl= length $ fc_inputs circuit
	        stableV graph circuit =head $ foldl (\acc (n,l) ->
					if take nl l == replicate nl '0' && ((==[]).catMaybes . fmap (\(_,_,l2)-> if l2 `elem` (fc_inputs circuit)
												then Nothing
												else Just l) $ out graph n ) 
						then l:acc
						else acc   ) [] $ labNodes graph
----   The edges removed if duplicates exist are arbitrary.
--mkSimple :: (DynGraph gr) => gr a b -> gr a b
--mkSimple = gmap simplify
--    where
--      rmLoops n = filter ((/=) n . snd)
--      rmDups = nubBy ((==) `on` snd)
--      simpleEdges n = rmDups . rmLoops n
--      simplify (p,n,l,s) = (p',n,l,s')
--          where
--            p' = simpleEdges n p
--            s' = simpleEdges n s
--
----findCycles :: (DynGraph g) => g a b -> Maybe ([NGroup], g a b)
--findCycles g
--    | isEmpty g = Nothing
--    | otherwise = Just . getCycles . matchAny $ g
--    where
--      getCycles (ctx,g') = (cyclesFor (ctx, g'), g')
--
---- | Find all cycles for the given node.
----pathTree             :: (DynGraph g) => Decomp g a b -> [NGroup]
--pathTree (Nothing,_) = []
--pathTree (Just ct,g)
--    | isEmpty g = []
--    | null sucs = [[n]]
--    | otherwise = (:) [n] . map (n:) . concatMap (subPathTree g') $ sucs
--    where
--      n = node' ct
--      sucs = suc' ct
--      -- Avoid infinite loops by not letting it continue any further
--      ct' = makeLeaf ct
--      g' = ct' & g
--      subPathTree gr n' = pathTree $ match n' gr
--
--addLabels    :: (Graph g) => g a b -> [Node] -> [LNode a]
--addLabels gr = map (ap (,) (fromJust . lab gr))
--
---- | Remove all outgoing edges
----makeLeaf           :: Context a b -> Context a b
--makeLeaf (p,n,a,_) = (p', n, a, [])
--    where
--      -- Ensure there isn't an edge (n,n)
--      p' = filter (\(_,n') -> n' /= n) p
--
--
--cyclesIn' = concat . unfoldr findCycles . mkSimple
--
----cyclesIn   :: (DynGraph g) => g a b -> [LNGroup a]
--cyclesIn g = map (addLabels g) (cyclesIn' g)
--
---- | Find all cycles in the given graph, returning just the nodes.
----cyclesIn' :: (DynGraph g) => g a b -> [NGroup]
--
--single [_] = True
--single  _  = False
----cyclesFor :: (DynGraph g) => GDecomp g a b -> [NGroup]
--cyclesFor = map init .
--            filter isCycle .
--            pathTree .
--            first Just
--    where
--      isCycle p = Prelude.not (single p) && (head p == last p)
--
main =do
	lArgs <- getArgs
	result <-parseFromFile netlistParser . (!!0) $ lArgs
	myEdges <-parseFromFile solutionParser . (!!1) $ lArgs
	case (result,myEdges)  of
		(Right b,Right myE) -> do
				let sg = SG.computeTransitionByCircuit . addIntermediateVariables $b in	
					let  csg = convertGraph sg in
					let graph = ((mkGraph (labNodes csg) .fmap 
							(\(x,y) -> (vertexOf csg x, vertexOf csg y,""))
							$ myE) :: Gr String String) in	
					let compo = last .  sortWith length . DFS.scc $ graph in
					let graphF = addLabels1 
									((fmap (\x->(x,undefined)) $c_inputs b\\ c_outputs b)
									 ++fc_eqs (addIntermediateVariables b)
									)
						 		$ delNodes (nodes graph \\ compo) graph in 
					do
						--putStrLn . show $ cyclesIn graphF 
						putStrLn . outputCircuitGraph (addIntermediateVariables b) $ graphF   
		_-> putStrLn "You should go to hell. Two times."


addLabels1 l g = gmap (\(l1,node,index,l2)->(fmap (\(_,before)->(edgeLabel l g before node,before)) l1,node,index,fmap (\(_,after)->(edgeLabel l g node after,after)) l2)) g   

edgeLabel l g a b =  fst . (l !!). fromJust . findIndex (\(x,y)-> x /=y )$ (fromJust . lab g $ a) `zip` (fromJust.lab g $ b) 
