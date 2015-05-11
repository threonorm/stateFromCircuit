{-# LANGUAGE TupleSections#-}

--module Reconstruction where

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
import Logic hiding (main)

import Data.GraphViz 
import Data.GraphViz.Printing hiding (char)

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import Data.Text.Lazy (unpack)
import System.Environment


chunkParser = manyTill anyChar . try $ 	
	keyword "v"


oneEdge = punctuation 'E' *> many (char '0' <|> char '1') <* spaces 
--I don't compute anything, I just cut the string in the middle
edgeParser =(\x -> let sz = length x in
			(take (sz `quot` 2) $ x, drop (sz `quot` 2) $ x)) <$> try ((try .many $ punctuation '-' *>oneEdge) *> oneEdge <* spaces )

solutionParser = chunkParser *> ( many $edgeParser)  <* ((try. many $ punctuation '-' *> oneEdge) <* keyword "c") 


main =do
	lArgs <- getArgs
	result <-parseFromFile netlistParser . (!!0) $ lArgs
	myEdges <-parseFromFile solutionParser . (!!1) $ lArgs
	case (result,myEdges)  of
		(Right b,Right myE) -> do
				let sg = computeTransitionByCircuit . addIntermediateVariables $b in	
					let  csg = convertGraph sg in
					do
						putStrLn . unpack . renderDot . toDot .graphToDot nonClusteredParams $ ((mkGraph (labNodes csg) .fmap (\(x,y) -> (vertexOf csg x, vertexOf csg y,"")) $ myE) :: Gr String String)	
		_-> putStrLn "You should go to hell. Two times."
