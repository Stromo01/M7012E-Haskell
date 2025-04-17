module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Data.Binary.Get (skip)
import Distribution.Simple.Setup (BuildFlags(BuildFlags))
type T = Statement
data Statement =
    Assignment String Expr.T |  -- variable ':=' expr ';'
    Skip |                      -- 'skip' ';'
    Begin [Statement] |         -- 'begin' statements 'end'
    If Expr.T Statement Statement |  -- 'if' expr 'then' statement 'else' statement
    While Expr.T Statement |    -- 'while' expr 'do' statement
    Read String |               -- 'read' variable ';'
    Write Expr.T                -- 'write' expr ';'
    deriving Show

newtype Statements = Statements [Statement] deriving Show
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipStmt = accept "skip" -# require ";" >-> const Skip
beginStmt = accept "begin" -# iter parse #- require "end" >-> Begin

ifStmt = (accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse) >-> buildIf
buildIf ((cond, thenStmts), elseStmts) = If cond thenStmts elseStmts

whileStmt = (accept "while" -# Expr.parse #- require "do" # parse) >-> buildWhile
buildWhile (cond, stmt) = While cond stmt

readStmt = accept "read" -# word #- require ";" >-> Read
writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment !
          skipStmt !
          beginStmt !
          ifStmt !
          whileStmt !
          readStmt !
          writeStmt !
          err "illegal statement"
  toString = error "Statement.toString not implemented"
