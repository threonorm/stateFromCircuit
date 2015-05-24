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
import FOL
import qualified StateGeneration as SG
import Logic

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import GHC.Exts (sortWith)

import qualified Data.Graph.Inductive.Query.DFS as DFS
import Data.Text.Lazy (unpack)
import System.Environment




outputCircuitGraph circuit graph =
	".inputs " ++ concat (intersperse " " (fmap ("S" ++) $fc_inputs circuit)) ++   
	"\n.outputs " ++ concat (intersperse " " (fmap ("S" ++) $fc_outputs circuit)) ++
	"\n.internal " ++ concat (intersperse " " (fmap ("S" ++) $fc_intermediate circuit)) ++
	"\n.state graph\n" ++ concat (fmap (\(x,y,l) -> (("S" ++)  . fromJust . lab graph $ x) ++ l 
					++( ("S" ++) . fromJust.lab graph $ y) ++ "\n"  ) $ labEdges graph ) ++
	".marking {" ++ (take (fromIntegral $n) $  repeat '0') ++ "}\n.end"
	where n= length $ fc_inputs circuit ++ fc_outputs circuit ++ fc_intermediate circuit


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
					let compo = last . sortWith length . DFS.scc $ graph in
					let graphF = addLabels 
									((fmap (\x->(x,undefined)) $c_inputs b\\ c_outputs b)
									 ++c_eqs b
									)
						 		$ delNodes (nodes graph \\ compo) graph in 
					do
						putStrLn . outputCircuitGraph (addIntermediateVariables b) $ graphF   
		_-> putStrLn "You should go to hell. Two times."


addLabels l g = gmap (\(l1,node,index,l2)->(fmap (\(_,before)->(edgeLabel l g before node,before)) l1,node,index,fmap (\(_,after)->(edgeLabel l g node after,after)) l2)) g   

edgeLabel l g a b =  fst . (l !!). fromJust . findIndex (\(x,y)-> x /=y )$ (fromJust . lab g $ a) `zip` (fromJust.lab g $ b) 
