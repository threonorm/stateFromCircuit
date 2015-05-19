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
import StateGeneration
import Logic

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import System.Environment

main =do
	lArgs <- getArgs
	result <-parseFromFile netlistParser . (!!0) $ lArgs
	case result  of
		Left a -> undefined
		Right b -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $ b in	
					let  csg = convertGraph sg in
					do
--						putStrLn . prettify $ csg	
--						putStrLn . show . allIsomorphisms (normalize outputPersistency) $ csg 
						putStrLn $ variablesSat csg
						putStrLn $ "Subject to"
						putStrLn . show . pretty . normalize $ atom "S" [Var . take (fromIntegral $n_v sg) $  repeat '0' ] 	
						putStrLn . show . pretty . printSatFormulas (normalize outputPersistency) $ csg
						putStrLn . show . pretty . living $ csg 
						putStrLn . show . pretty . defineReachable $ csg 
					      	putStrLn . show . pretty . propagateSignals csg . fromIntegral .n_inputs $ sg      
						putStrLn $ "Binary"
						putStrLn .("\t"++). intercalate " " . fmap 
							(\(x,y)-> (\(a,b)->"E"++a++b).(\(a,b)->(fromJust a, fromJust b))
									$(lab csg x ,lab csg y))
							 $ Data.Graph.Inductive.Graph.edges csg
						putStrLn .("\t"++). intercalate " " . fmap 
							(\(x)-> (\(a)->"S"++a).fromJust
									$(lab csg x))
							 $ Data.Graph.Inductive.Graph.nodes csg	
						putStrLn $ "End"
