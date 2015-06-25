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
	targetSpec <- parseFromFile stateGraphStart . (!!1) $ lArgs 
	case (result, targetSpec)  of
		(Right b, Right target) -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $ b in	
					let  csg = convertGraph sg in
					let nin = fromIntegral . n_inputs $ sg in  
					let nout =  fromIntegral . n_outputs $ sg in 
					let inte =  fromIntegral (n_v sg)  - nin - nout in
					do
--						putStrLn . prettify $ csg	
--						putStrLn . show . allIsomorphisms (normalize outputPersistency) $ csg 
						putStrLn $ variablesSat csg
						putStrLn $ "Subject to"
						mapM_(\(x,y) -> putStrLn . show . pretty . normalize $ atom "E" [Var  x, Var y]) target	
						putStrLn . show . pretty . printSatFormulas (normalize outputPersistency2) csg nin $ inte 
						putStrLn . show . pretty . printSatFormulas (normalize inputCannotTrigger) csg nin $ inte 
						-- putStrLn . show . pretty . printSatFormulas (normalize inputCannotInput) csg nin $ inte  
						--putStrLn . show . pretty . printSatFormulas (normalize forwardPersistency2) csg nin $ inte  
						putStrLn . show . pretty . living $ csg 
						putStrLn . show . pretty . defineReachable $ csg 
					      	putStrLn . show . pretty . propagateSignals csg . fromIntegral .n_inputs $ sg      
						putStrLn . show . pretty . mutually $ csg
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
		_ -> undefined



constraintCycle :: [ String ] -> INF
constraintCycle l = normalize . foldl 
			(\acc (start,end)-> acc `FOL.and` atom "E" [Var start , Var end] )
			tt
			. zip l . tail $ l  
