{-# LANGUAGE TupleSections, FlexibleInstances #-}


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
import Logic 

import System.Environment

main =do
	lArgs <- getArgs
	result <-parseFromFile netlistParser $ lArgs !! 0
	case result  of
		Left a -> putStrLn "fail"
		Right b -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $b in	
					putStrLn . unpack . renderDot . toDot .graphToDot nonClusteredParams $ convertGraph sg 

