module Razor where

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp deriving (Show, Eq)

eval :: Exp -> Int
eval (Lit x) = x
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

pretty :: Exp -> String
pretty (Lit x) = show x
pretty (Add exp1 exp2) = "( " ++ (pretty exp1) ++ " + " ++ (pretty exp2) ++ " )"
pretty (Mul exp1 exp2) = "( " ++ (pretty exp1) ++ " * " ++ (pretty exp2) ++ " )"

distribute :: Exp -> Exp
distribute _ = undefined
--
-- Mul (Add (Lit 1) (Lit 2)) Lit 5 == Add (Mul (Lit 5) (Lit 1)) (Mul (Lit 5) (Lit 2))