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
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
skipStatement = accept "skip" -# require ";" >-> \_ -> Skip
beginStatement = accept "begin" -# iter parse #- require "end" >-> Begin
ifStatement = accept "if" -#  Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
whileStatement = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
readStatement = accept "read" -# word #- require ";" >-> Read
writeStatement = accept "write" -# Expr.parse #- require ";" >-> Write
commentStatement = accept "--" -# iter (char ? (/= '\n')) #- require "\n" >-> Comment


buildAss (v, e) = Assignment v e
buildIf ((expr, doIf), doElse) = If expr doIf doElse
buildWhile (expr, stmt) = While expr stmt

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment str expr : ts) dict input = exec ts (Dictionary.insert (str, Expr.value expr dict) dict) input
exec (Skip : ts) dict input = exec ts dict input
exec (Begin stmts : ts) dict input = exec (stmts ++ ts) dict input
exec (If cond thenStmts elseStmts : stmts) dict input = 
    if (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt : ts) dict input = 
    if (Expr.value cond dict) > 0
    then exec (stmt : (While cond stmt) : ts) dict input
    else exec ts dict input
exec (Read str : ts) dict (i:input) = exec ts (Dictionary.insert (str, i) dict) input
exec (Write expr : ts) dict input = Expr.value expr dict : exec ts dict input
exec (Comment _ : ts) dict input = exec ts dict input

indent = flip replicate '\t'

shw :: Int -> T -> String
shw n (Assignment v e) = indent n ++ v ++ " := " ++ toString e ++ ";" ++ "\n"
shw n (Skip) = indent n ++ "skip" ++ ";" ++ "\n"
shw n (Begin ss) = indent n ++ "begin\n" ++ concat [shw (n+1) s | s <- ss] ++ indent n ++ "end"
shw n (If e s1 s2) = indent n ++ "if " ++ toString e ++ " then\n" ++ shw (n+1) s1 ++ indent n ++ "else\n" ++ shw (n+1) s2
shw n (While e s) = indent n ++ "while " ++ toString e ++ " do\n" ++ shw (n+1) s ++ "\n"
shw n (Read v) = indent n ++ "read " ++ v ++ ";" ++ "\n"
shw n (Write e) = indent n ++ "write " ++ toString e ++ ";" ++ "\n"
shw n (Comment v) = indent n ++ "-- " ++ v ++ "\n"

instance Parse Statement where
  parse = assignment ! skipStatement ! beginStatement ! ifStatement ! whileStatement ! readStatement ! writeStatement ! commentStatement
  toString = shw 0
