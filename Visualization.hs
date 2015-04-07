{-# LANGUAGE TupleSections, FlexibleInstances #-}

--module Visualization where

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
import Data.Text.Lazy
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import Data.GraphViz 
import Data.GraphViz.Printing
import Data.Graph.Inductive.Example
import StateGeneration
import AST
import Checking
import Parser


instance PrintDot (Vector Bool) where
	unqtDot = unqtDot . List.concat. List.map (\x-> if x then "1" else "0") . toList 

createGraph sg = 
	graphElemsToDot nonClusteredParams vertex es
	where 
		vertex = List.map (\x -> (x,myshow x)) . Map.keys . edges $ sg
		es = Map.foldWithKey 
			(\k (v1,v2) accM ->List.foldl 
						(\acc v->
							(k, flipV k v,show v++"-"):acc) 
						(List.foldl 
							(\acc v->
								(k,flipV k v,show v++"+"):acc) 
							accM
							v1)
						v2)
			[] 
			$ edges sg 
		flipV vertex index = imap (\i b -> if i== fromInteger index then not b else b) vertex  


myshow = List.map (\x-> if x then '1' else '0' ) . toList
graphToDotPng fpre g = --handle (\(e::GraphvizException) -> return False)
                       runGraphviz (createGraph g) Jpeg fpre >> return ()

main =do
	myLine <- getLine
	result <-parseFromFile netlistParser myLine
	case result  of
		Left a -> putStrLn "fail"
		Right b -> do
				putStrLn . unpack . renderDot . toDot -- $ graphToDot nonClusteredParams clr479 
				 . createGraph . computeTransitionByCircuit . addIntermediateVariables $b --"tests/test2"


