{-# LANGUAGE TupleSections#-}

module Parser where

import Control.Monad
import Control.Exception (assert)
import Control.Applicative hiding ((<|>), many)
import Data.Char
import Data.List
import Data.Functor
import qualified Data.Map as Map

import Text.Parsec 
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

-- Be careful with keywords which are not identifiers
-- Because of the whitespaces are significatives, I need
-- two alternatives versions, the first one eat the right-trailing
-- whitespaces, the second one eat the left-trailing whitespaces
-- This is a painful complication

 
ident = try $ do
  foo <- token1 ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` [".inputs", ".outputs"]
    then mzero
    else return foo

ident2 = try $ do
  foo <- token2 ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` [".inputs", ".outputs"]
    then mzero
    else return foo


	-- b) End of line
eodecl = punctuation ';'

	-- c) Comments TODO

-- II. Headers

inputs = bigList ".inputs" ident <* eodecl
outputs = bigList ".outputs" ident <* eodecl

-- III. Equations

equations = many $ equation <* eodecl

equation = do z <- ident
              punctuation '='
              e <- expr
              return (z, e)

-- IV. Expressions 

expr    = buildExpressionParser table term

-- The next function will change if I add new operators

term    =  do { punctuation2 '(' ; x <- expr ; punctuation2 ')' ; return x } 
	<|> Earg <$> ident2

table   = 	[[prefix "!" (Enot), prefix "~" (Enot), postfix "'" (Enot)]
		,[Infix (try $ do{ string $ " ";  -- " " mean AND in some contexts
			 	try . token2 . string $ ""; 
				lookAhead expr;
				return (Ebinop And) })
			 AssocLeft]
		,[binary "&" (Ebinop And) AssocLeft]
	  	,[binary "+" (Ebinop Or) AssocLeft]]
        
binary  name fun assoc = Infix (try $ do{  try . token1 . token2 . string $ name; return fun }) assoc
prefix  name fun       = Prefix (try $do{ try . token2 . string $ name; return fun })
postfix  name fun       = Postfix (try $do{ try . token2 . string $ name; return fun })

-- Useful parsing tools

keyword x = token1 . try $ string x *> notFollowedBy identChar

-- The fact that a whitespace can be a AND make it painful, we need to
-- sanitize to the left or to the right depending on the context
token1 t = t <* spaces
token2 t = spaces *> t 

punctuation = token1 . char
punctuation2 = token2 . char

bigList header eltParser = keyword header *> many eltParser  


