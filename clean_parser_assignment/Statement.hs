module Statement (T, parse, toString, fromString, exec) where

import Data.Binary.Get (skip)
import Dictionary qualified
import Distribution.Simple.Setup (BuildFlags (BuildFlags))
import Expr qualified
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T -- variable ':=' expr ';'
  | Skip -- 'skip' ';'
  | Begin [Statement] -- 'begin' statements 'end'
  | If Expr.T Statement Statement -- 'if' expr 'then' statement 'else' statement
  | While Expr.T Statement -- 'while' expr 'do' statement
  | Read String -- 'read' variable ';'
  | Write Expr.T -- 'write' expr ';'
  deriving (Show)

newtype Statements = Statements [Statement] deriving (Show)

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss (v, e) = Assignment v e

skipStmt :: Parser Statement
skipStmt = accept "skip" -# require ";" >-> const Skip

beginStmt :: Parser Statement
beginStmt = accept "begin" -# iter parse #- require "end" >-> Begin

ifStmt :: Parser Statement
ifStmt = (accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse) >-> buildIf

buildIf :: ((Expr.T, Statement), Statement) -> Statement
buildIf ((cond, thenStmts), elseStmts) = If cond thenStmts elseStmts

whileStmt :: Parser Statement
whileStmt = (accept "while" -# Expr.parse #- require "do" # parse) >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (cond, stmt) = While cond stmt

readStmt :: Parser Statement
readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt :: Parser Statement
writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts : stmts) dict input =
  if (Expr.value cond dict) > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input

parseStatement :: Parser Statement
parseStatement = assignment ! skipStmt ! beginStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt

instance Parse Statement where
  parse = parseStatement
  toString = error "Statement.toString not implemented"
