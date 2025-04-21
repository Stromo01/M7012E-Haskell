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
exec [] _ _ = []

exec (If cond thenStmts elseStmts : stmts) dict input =
  if (Expr.value cond dict) > 0
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input

exec (Skip : stmts) dict input = exec stmts dict input -- Skip, continue with the next statement

exec (Begin innerStmts : stmts) dict input = exec (innerStmts ++ stmts) dict input -- execute all statements in the block, then continue

exec (While cond stmt : stmts) dict input =
  if (Expr.value cond dict) > 0
    then exec (stmt : While cond stmt : stmts) dict input -- execute the statement and check the condition again
    else exec stmts dict input -- condition is false, continue with the next statement

exec (Assignment var expr : stmts) dict input =
  let value = Expr.value expr dict
      newDict = Dictionary.insert (var, value) dict -- update the dictionary with the new value
   in exec stmts newDict input -- continue with the next statement

exec (Read var : stmts) dict (i : input) = exec stmts (Dictionary.insert (var, i) dict) input

exec (Write expr : stmts) dict input =
  let value = Expr.value expr dict
   in value : exec stmts dict input -- output the value and continue with the next statement

   
instance Parse Statement where
  parse = assignment ! skipStmt ! beginStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt
  toString = error "Statement.toString not implemented"
