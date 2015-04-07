{-# LANGUAGE TupleSections#-}

module StateGeneration where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import qualified Data.List as List
import Data.Maybe
import Data.Vector hiding ((++))
import Data.Functor
import qualified Data.Map as Map
import Debug.Trace

import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Checking
import Parser

type Vertex = Vector Bool --Of size fixed 

-- inputs | outputs mixed with intermediates so n_v= n_int + n_inp + n_outpu
data StateGraph =
	SG { n_v :: Integer,
		n_inputs :: Integer,
		n_outputs :: Integer,
		edges :: Map.Map Vertex ([Integer],[Integer])			
	}		
	deriving (Show)


-- The map keep track on [GoUp],[GoDowm]
-- Generate the 2^n states with list and then convert to Vectors to
-- gain time

generateListVertexList :: Integer -> [[Bool]]
generateListVertexList 1 = [ [True], [False] ]
generateListVertexList n = let prev = generateListVertexList (n-1) in
					(List.map (\x -> False:x) prev) List.++ (List.map (\x -> True:x) prev)
	
generateListVertexVector :: Integer -> [ Vector Bool ]
generateListVertexVector n = List.map fromList $ generateListVertexList n

-- Generate a graph with all the vertex needed, and then we will add the edges

generateEmptyGraph n i o= SG n i o. Map.fromList . List.map (\x -> (x,([],[]))) $ generateListVertexVector n

computeTransitionByCircuit :: FullCircuit -> StateGraph
computeTransitionByCircuit fc =
	List.foldl 
		(\acc x ->
			addInputsChanges 
				x -- I can add everyone in one shot?
				$ List.foldl
					(\acc l ->
					if (x ! fromInteger (i+l) ) /= evaluate fc x (fromInteger l)  
						then changeS (fromInteger (i+l)) x acc 
						else acc
					)
					acc
					$ [0.. (toInteger . List.length $ fc_eqs fc)-1 ]--((List.length $fc_eqs fc)-1)]
		)
		emptyGraph
		(Map.keys . edges $ emptyGraph ) 
	where 	emptyGraph = generateEmptyGraph  (v + i + o) i  o   
		i = toInteger . List.length $ fc_inputs fc
		o = toInteger . List.length $ fc_outputs fc
		v = toInteger . List.length $ fc_intermediate fc
		addInputsChanges v sg = List.foldl (\acc x -> acc{edges = Map.adjust 
								(\(a,b) -> if v ! (fromInteger x) then (a,x:b)
										else (x:a,b))
								 v
								 $ edges acc}) 
						sg 
						$ [0..(i - 1)] 
		evaluate fc x i = evaluateExp (snd $ (fc_eqs fc) List.!! i) fc x --Next is unsafe with fromJust but it has been checked before
		evaluateExp (Earg s) fc x = --Debug.Trace.trace (s ++ "\n"++show fc ++"\n" ++ show x  )$ x !
						x!( fromJust . List.elemIndex s $ fc_inputs fc ++ (List.map fst $fc_eqs fc))  
		evaluateExp (Enot e) fc x = not(evaluateExp e fc x)  
		evaluateExp (Ebinop b e1 e2) fc x = case b of
			Or -> (evaluateExp e1 fc x) || (evaluateExp e2 fc x)
			And -> (evaluateExp e1 fc x) && (evaluateExp e2 fc x)
		changeS i x acc = acc{edges = Map.adjust 
						(\(a,b) -> if x ! i then (a,(toInteger i):b)
									else ((toInteger i):a,b)) 
						x 
						$ edges acc } 




--main :: IO()
--main =do
--	putStrLn "start" 
--	myLine <- getLine
--	result <-parseFromFile netlistParser myLine
--	case result  of
--		Left a -> putStrLn "fail"
--		Right b -> do
--				putStrLn . show . checkSanityFullCircuit . addIntermediateVariables $ b
--				putStrLn. show . computeTransitionByCircuit . addIntermediateVariables $ b
--
--

