-- Haskell is space sensitive
{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-tabs #-}

-- Turn on warnings

--
-- You can start GHCi at the UNIX prompt with the command `stack ghci`.
--
-- You can load this file into GHCi by typing the command `:load Examples.hs` at
-- the GHCi prompt.

module Examples where

type Var = String

data Exp
  = Var Var
  | Const Double
  | Neg Exp
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Show)

-- | Evaluate expressions
eval :: [(Var, Double)] -> Exp -> Double
eval env (Var v) = case lookup v env of
  Nothing -> error $ "Unbound variable: " ++ v
  Just n -> n
eval _ (Const n) = n
eval env (Neg e) = -(eval env e)
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2

-- Compute derivative of an expression with respect to a variable
deriv :: Exp -> Var -> Exp
deriv (Var v) x
  | v == x = Const 1
  | otherwise = Const 0
deriv (Const _) _ = Const 0
deriv (Neg e) x = Neg (deriv e x)
deriv (Add e1 e2) x = Add (deriv e1 x) (deriv e2 x)
deriv (Sub e1 e2) x = Sub (deriv e1 x) (deriv e2 x)
deriv (Mul e1 e2) x =
  Add
    (Mul e1 (deriv e2 x))
    (Mul (deriv e1 x) e2)

-- | "Compile" an expression into a Haskell function
compile :: Exp -> Double -> Double
compile e x = eval [("x", x)] e
