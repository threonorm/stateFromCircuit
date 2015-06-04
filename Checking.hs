{-# LANGUAGE TupleSections#-}

module Checking where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Data.Functor
import qualified Data.Map as Map
import qualified Debug.Trace as D
import Text.Parsec 
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
 
import AST
import Parser

addIntermediateVariables :: Circuit -> FullCircuit
addIntermediateVariables circuit = 
	Fc ((c_inputs circuit) \\ (c_outputs circuit) )
		(filter (\x -> take 3 x /= "csc" ) $ c_outputs circuit) 
		(putAt interm)
		$ (putAtTheEnd $ c_eqs circuit) --USEFUL : put the output at the end  
	where 	
		interm = ((nub . concat . map (extractVariables . snd) $ c_eqs circuit)
			\\ (filter (\x -> take 3 x /= "csc") $ c_outputs circuit))
			\\ (c_inputs circuit \\ c_outputs circuit)
		extractVariables (Earg a) = [a]
		extractVariables (Enot a) = extractVariables a
		extractVariables (Ebinop _ a b) = extractVariables a
						++ extractVariables b
		putAtTheEnd y =  (filter (\(x,_) -> take 3 x == "csc" ) y) ++ (filter (\(x,_) -> take 3 x /= "csc") y)  
		putAt y =  (filter (\(x) -> take 3 x == "csc" ) y) ++ (filter (\(x) -> take 3 x /= "csc") y)  
-- TODO Right now it is with csc I should check inside outputs


-- We need to check that every output is defined
-- That nobody is defined two times
-- That everyone that is used is defined or is an input 
-- TODO for now I don't check that there are no the same input/output
-- multiple times... TODO?
checkSanityFullCircuit :: FullCircuit -> Bool
checkSanityFullCircuit fc =
	nub (map fst eqs) == map fst eqs &&
	sort needToDefine == (sort . map fst) eqs 
	where 	eqs = fc_eqs fc 	
		needToDefine = fc_outputs fc ++  fc_intermediate fc   





--main :: IO()
--main =do
--	putStrLn "start" 
--	myLine <- getLine
--	result <-parseFromFile netlistParser myLine
--	case result  of
--		Left a -> putStrLn "fail"
--		Right b -> do
--				putStrLn . show . addIntermediateVariables $ b
--				putStrLn . show . checkSanityFullCircuit . addIntermediateVariables $ b
--

  
