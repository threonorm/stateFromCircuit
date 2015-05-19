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
import Data.Text.Lazy (pack)
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Checking
import Parser
import FOL
import StateGeneration
import Logic 

import Data.GraphViz 
import Data.GraphViz.Printing hiding (char)

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import Data.Text.Lazy (unpack)
import System.Environment


main =do
	lArgs <- getArgs
	result <-parseFromFile netlistParser . (!!0) $ lArgs
	myEdges <-parseFromFile solutionParser . (!!1) $ lArgs
	case (result,myEdges)  of
		(Right b,Right myE) -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $b in	
					let  csg = convertGraph sg in
					do
						putStrLn . unpack . renderDot . toDot .graphToDot nonClusteredParams {fmtNode = \(_,x)-> [textLabel $ pack x] ,fmtEdge = \(x,y,z) -> [textLabel $ pack z]} $ ((addLabels (c_eqs b) $ mkGraph (labNodes csg) .fmap (\(x,y) -> (vertexOf csg x, vertexOf csg y,"")) $ myE) :: Gr String String)	
		_-> putStrLn "You should go to hell. Two times."


addLabels l g = gmap (\(l1,node,index,l2)->(fmap (\(_,before)->(edgeLabel l g before node,before)) l1,node,index,fmap (\(_,after)->(edgeLabel l g node after,after)) l2)) g   

edgeLabel l g a b =  fst . (l !!). fromJust . findIndex (\(x,y)-> x /=y )$ (fromJust . lab g $ a) `zip` (fromJust.lab g $ b) 
