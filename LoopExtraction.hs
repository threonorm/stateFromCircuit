{-# LANGUAGE TupleSections, OverloadedStrings#-}

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

import Data.Text (pack)
import System.Environment
import Turtle

nextNotUsed [] =  return ExitSuccess
nextNotUsed (t:q) = do
	lArgs<- getArgs
	_ <- shell (pack $ script ((!!1) lArgs) t) Turtle.empty  
	_ <- cp (fromText . pack $ ((!!1) lArgs) ++ ".csg")  (fromText . pack $((!!1) lArgs) ++ show t ++".csg")  
	myE<- parseFromFile solutionParser $  ((!!1) lArgs)++ ".solReadable" 
	case myE of
		Right myEdges -> nextNotUsed ((t:q) \\ (fmap (\(x,y)-> event x y)  myEdges)) 
		Left _ -> return $ ExitFailure 1  
script v1 t =
	"./StateFromCircuit/Sat "++ v1 ++" | sed \"s/;//\" > "++ v1 ++".lp\n"++ --add the line of 
	"./gurobi.sh gurosolve.py " ++ v1 ++ 
	"\ncat " ++ v1 ++ ".sol | grep -v \" 0\" | grep -v \"#\" | sed \"s/ 1//\" | sort > "++ v1 ++ ".solReadable"++ 
	"\n./StateFromCircuit/Printer " ++ v1 ++ " "++ v1 ++".solReadable > " ++ v1 ++ ".csg"


main = nextNotUsed [0..2]
