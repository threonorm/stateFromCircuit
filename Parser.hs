{-# LANGUAGE TupleSections#-}

module Parser where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Data.Functor
import qualified Data.Map as Map
import Debug.Trace
import Text.Parsec hiding (token)
import Text.Parsec.String
import Text.Parsec.Expr 
import Text.Parsec.Token
 
import AST

------------------
-- The language --
------------------

-- A - Main structure of a file

netlistParser :: Parser Circuit 
netlistParser = spaces *> netlist <* spaces <* eof

netlist = C <$> inputs <*> outputs <*> equations

-- I. Definitions

	-- a) Identifiers
identStartChar = letter <|> char '_'
identChar = identStartChar <|> digit 


 
iden = try $ do
  foo <- token ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` ["INORDER", "OUTORDER"]
    then mzero
    else return foo

ident = iden <|> do {punctuation '['; l<-iden ; punctuation ']'; return l; }  


	-- b) End of line
eodecl = punctuation ';'

	-- c) Comments TODO

-- II. Headers

inputs = bigList "INORDER =" ident <* eodecl
outputs =bigList "OUTORDER =" ident <* eodecl

-- III. Equations

equations = many $ equation 

equation = try $ do
		z <- ident
		punctuation '='
       		e <- expr <* eodecl
              	return (z, e)

-- IV. Expressions 



expr    =(try $ buildExpressionParser table term)

-- The next function will change if I add new operators

term    = 
	 (try $ do { punctuation '(' ; x <- expr; punctuation ')' ; return x })
	<|> Earg <$> ident
	
table   = 	[[prefix "!" (Enot), prefix "~" (Enot), postfix "'" (Enot)]
		,[binary "&" (Ebinop And) AssocLeft]
		,[binary "" (Ebinop And) AssocLeft]
	  	,[binary "+" (Ebinop Or) AssocLeft]]
        
binary  name fun assoc = Infix (try $ do{  try . token . string $ name; return fun }) assoc
prefix  name fun       = Prefix (try $do{ try . token . string $ name; return fun })
postfix  name fun       = Postfix (try $do{ try . token . string $ name; return fun })

-- Useful parsing tools

keyword x = token . try $ string x *> notFollowedBy identChar

-- The fact that a whitespace can be a AND make it painful, we need to
-- sanitize to the left or to the right depending on the context
token t = t <* spaces

punctuation = token . char

bigList header eltParser = keyword header *> many eltParser  

-- Reconstitution parser miniSat

chunkParser = manyTill anyChar . try $ 	
	keyword "v"


oneEdge = punctuation 'E' *> many (char '0' <|> char '1') <* spaces 
--I don't compute anything, I just cut the string in the middle
edgeParser =(\x -> let sz = length x in
			(take (sz `quot` 2) $ x, drop (sz `quot` 2) $ x)) <$> try ((try .many $ punctuation '-' *>oneEdge) *> oneEdge <* spaces )

solutionParserMinisat = chunkParser *> ( many $edgeParser)  <* ((try. many $ punctuation '-' *> oneEdge) <* keyword "c") 

-- Reconstitution parser gurobi
solutionParser = many $ (\x -> let sz = length x in
			(take (sz `quot` 2) $ x, drop (sz `quot` 2) $ x)) <$> oneEdge

 
