module Razor where

data Exp = Lit Int | Add Exp Exp | Mul Exp Exp deriving (Show, Eq)

eval :: Exp -> Int
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

pretty :: Exp -> String
pretty (Lit x) = show x
pretty (Add x y) = "( " ++ pretty x ++ " + " ++ pretty y ++ " )"
pretty (Mul x y) = "( " ++ pretty x ++ " * " ++ pretty y ++ " )"

distribute :: Exp -> Exp
distribute x@(Lit _) = x
distribute (Add x y) = Add (distribute x) (distribute y)
distribute (Mul (Add x y) z) = Add (Mul (distribute x) (distribute z)) (Mul (distribute y) (distribute z))
distribute (Mul x (Add y z)) = Add (Mul (distribute y) (distribute x)) (Mul (distribute z) (distribute x))
distribute (Mul x y) = Mul (distribute x) (distribute y)
  
--
-- Mul (Add (Lit 1) (Lit 2)) Lit 5 == Add (Mul (Lit 5) (Lit 1)) (Mul (Lit 5) (Lit 2))