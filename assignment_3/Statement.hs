module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin  [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
skipStatement = accept "skip" -# require ";" >-> \_ -> Skip
beginStatement = accept "begin" -# iter parse #- require "end" >-> Begin
ifStatement = accept "if" -#  Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
readStatement = accept "read" -# word #- require ";" >-> Read
writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write


buildAss (v, e) = Assignment v e
buildIf ((expr, doIf) doElse) = if expr doIf doElse
buildWhile (expr, stmt) -> While expr stmt

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! skipStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement
