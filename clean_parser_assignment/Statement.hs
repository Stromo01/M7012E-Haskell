module Statement (T, parse, toString, fromString, exec) where

import Data.Binary.Get (skip)
import Dictionary qualified
import Distribution.Simple.Setup (BuildFlags (BuildFlags))
import Expr qualified
import Parser hiding (T)
import Prelude hiding (fail, return)
import GHC.Generics (Generic(Rep))

type T = Statement

data Statement
  = Assignment String Expr.T -- variable ':=' expr ';'
  | Skip -- 'skip' ';'
  | Begin [Statement] -- 'begin' statements 'end'
  | If Expr.T Statement Statement -- 'if' expr 'then' statement 'else' statement
  | While Expr.T Statement -- 'while' expr 'do' statement
  | Read String -- 'read' variable ';'
  | Write Expr.T -- 'write' expr ';'
  | Repeat Statement Expr.T -- 'repeat' statements 'until' expr ';'
  deriving (Show)

newtype Statements = Statements [Statement] deriving (Show)

assignment :: Parser Statement -- variable ':=' expr ';'
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

skipStmt :: Parser Statement -- 'skip' ';'
skipStmt = accept "skip" -# require ";" >-> const Skip

beginStmt :: Parser Statement -- 'begin' statements 'end'
beginStmt = accept "begin" -# iter parse #- require "end" >-> Begin

ifStmt :: Parser Statement -- 'if' expr 'then' statement 'else' statement
ifStmt = (accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse) >-> buildIf

buildIf :: ((Expr.T, Statement), Statement) -> Statement -- ((expr, thenStmt), elseStmt) -> If expr thenStmt elseStmt
buildIf ((cond, thenStmts), elseStmts) = If cond thenStmts elseStmts

whileStmt :: Parser Statement -- 'while' expr 'do' statement
whileStmt = (accept "while" -# Expr.parse #- require "do" # parse) >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (cond, stmt) = While cond stmt

readStmt :: Parser Statement -- 'read' variable ';'
readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt :: Parser Statement -- 'write' expr ';'
writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

repeatStmt :: Parser Statement -- 'repeat' statements 'until' expr ';'
repeatStmt = (accept "repeat" -# parse #- require "until" # Expr.parse #- require ";") >-> buildRepeat

buildRepeat :: (Statement, Expr.T) -> Statement
buildRepeat (stmt, cond) = Repeat stmt cond

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (If cond thenStmts elseStmts : stmts) dict input =
  if Expr.value cond dict > 0                 -- condition is true
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

exec (Repeat stmt cond : stmts) dict input =
  exec (stmt : If cond Skip (Repeat stmt cond) : stmts) dict input


instance Parse Statement where
  parse = assignment ! skipStmt ! beginStmt ! ifStmt ! whileStmt ! readStmt ! writeStmt ! repeatStmt -- parse all statements
  toString (Assignment var expr) = var ++ " := " ++ Expr.toString expr ++ ";\n" 
  toString Skip = "skip;\n"
  toString (Begin stmts) = "begin\n" ++ concatMap toString stmts ++ "end\n"
  toString (If cond thenStmt elseStmt) =
    "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmt ++ "else\n" ++ toString elseStmt
  toString (While cond stmt) =
    "while " ++ Expr.toString cond ++ " do\n" ++ toString stmt
  toString (Read var) = "read " ++ var ++ ";\n"
  toString (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
  toString (Repeat stmts cond) =
    "repeat\n" ++ toString stmts ++ "until" ++ Expr.toString cond ++ "\n"
