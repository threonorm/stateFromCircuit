{-# LANGUAGE TupleSections, TypeOperators, EmptyDataDecls, KindSignatures #-}

--module Parser where

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

netlistParser :: Parser Circuit 
netlistParser = spaces *> netlist <* spaces <* eof


netlist = Pr <$> inputs <*> outputs 
             <*> equations


token1 t = t <* spaces
token2 t = spaces *> t 
punctuation = token1 . char
punctuation2 = token2 . char

identStartChar = letter <|> char '_'
identChar = identStartChar <|> digit 

eodecl = punctuation ';'

keyword x = token1 . try $ string x *> notFollowedBy identChar


ident = try $ do
  foo <- token1 ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` [".inputs", ".outputs"]
    then mzero
    else return foo

bigList header eltParser = keyword header
				   *> many eltParser   --`sepBy` (punctuation ',')

inputs = bigList ".inputs" ident <* eodecl
outputs = bigList ".outputs" ident <* eodecl

equations = many $ equation <* eodecl

equation = do z <- ident
              punctuation '='
              e <- expr
              return (z, e)


---------------------------
-- Parser for expressions 
---------------------------


ident2 = try $ do
  foo <- token2 ( (:) <$> identStartChar <*> many identChar )
  if foo `elem` [".inputs", ".outputs"]
    then mzero
    else return foo

expr    = buildExpressionParser table term

-- The next function will change if I add new operator
-- I need the list of all the binaries operators 

term    =  do { punctuation2 '(' ; x <- expr ; punctuation2 ')' ; return x } 
	<|> (Earg <$> ident2)

table   = [[prefix "!" (Enot), prefix "~" (Enot), postfix "'" (Enot)]
	,[Infix (try $ do{ string $ " ";
			 try . token2. string $ ""; 
			lookAhead expr;
			return (Ebinop And) }) AssocLeft]
	,[binary "&" (Ebinop And) AssocLeft]
	  , [binary "+" (Ebinop Or) AssocLeft ]]
        
binary  name fun assoc = Infix (try $ do{  try . token1 . token2 . string $ name; return fun }) assoc
prefix  name fun       = Prefix (try $do{ try . token2 . string $ name; return fun })

postfix  name fun       = Postfix (try $do{ try . token2 . string $ name; return fun })



   
main :: IO()
main =do
	putStrLn "start" 
	myLine <- getLine
	result <-parseFromFile netlistParser myLine
	case result  of
		Left a -> putStrLn "fail"
		Right b -> putStrLn . show $ b
