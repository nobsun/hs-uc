module Language.SIMPLE.TransitionSemantics (
   isRedex
  ,reduceExpr
  ,reduceSequence
  ,runExprMachine
  ,isReducible
  ,reduceStm
  ) where

import Control.Applicative
import Text.PrettyPrint
import Language.SIMPLE.AbstractSyntax
import Language.SIMPLE.Environment
import Language.SIMPLE.PrettyPrint
-- $setup
-- >>> type Name = String
-- >>> let exp0 = Add (Multiply (Number 1) (Number 2)) (Multiply (Number 3) (Number 4)) :: Expr Name
-- >>> let env0 = emptyEnv

isNumber :: Expr a -> Bool
isNumber expr = case expr of
    Number _ -> True
    _        -> False

isBoolean :: Expr a -> Bool
isBoolean expr = case expr of
    Boolean _ -> True
    _         -> False

isNormalForm :: Expr a -> Bool
isNormalForm = (||) . isNumber <*> isBoolean

-- |
-- Predicate if the specified expression is redex or not.
--
-- >>> isRedex (Number 1 :: Expr Name)
-- False
-- >>> isRedex (Boolean undefined :: Expr Name)
-- False
-- >>> isRedex (Variable undefined :: Expr Name)
-- True
-- >>> isRedex (Add undefined undefined :: Expr Name)
-- True
-- >>> isRedex (Multiply undefined undefined :: Expr Name)
-- True
-- >>> isRedex (And undefined undefined :: Expr Name)
-- True
-- >>> isRedex (Or undefined undefined :: Expr Name)
-- True
-- >>> isRedex (LessThan undefined undefined :: Expr Name)
-- True
-- >>> isRedex (Not undefined :: Expr Name)
-- True
isRedex :: Expr a -> Bool
isRedex = not . isNormalForm

isδredex :: Expr a -> Bool
isδredex (Add e1 e2)       = isNormalForm e1 && isNormalForm e2
isδredex (Multiply e1 e2)  = isNormalForm e1 && isNormalForm e2
isδredex (And e1 e2)       = isNormalForm e1 && isNormalForm e2
isδredex (Or e1 e2)        = isNormalForm e1 && isNormalForm e2
isδredex (LessThan e1 e2)  = isNormalForm e1 && isNormalForm e2
isδredex (Not e)           = isNormalForm e
isδredex _                 = False

δreduce :: Expr a -> Expr a
δreduce (Add (Number m) (Number n))      = Number (m+n)
δreduce (Multiply (Number m) (Number n)) = Number (m*n)
δreduce (LessThan (Number m) (Number n)) = Boolean (m<n)
δreduce (And (Boolean b) (Boolean c))    = Boolean (b&&c)
δreduce (Or (Boolean b) (Boolean c))     = Boolean (b||c)
δreduce (Not (Boolean b))                = Boolean (not b)
δreduce _                                = error "Type error"

-- |
-- make a small-step reducing of the specified expression in the specified environment.
-- 
-- >>> isRedex exp0
-- True
-- >>> let exp1 = reduceExpr emptyEnv exp0
-- >>> exp1
-- Add (Number 2) (Multiply (Number 3) (Number 4))
-- >>> isRedex exp1
-- True
-- >>> let exp2 = reduceExpr emptyEnv exp1
-- >>> exp2
-- Add (Number 2) (Number 12)
-- >>> isRedex exp2
-- True
-- >>> let exp3 = reduceExpr emptyEnv exp2
-- >>> exp3
-- Number 14
-- >>> isRedex exp3
-- False
reduceExpr :: (Ord a, Show a) => Env Expr a -> Expr a -> Expr a
reduceExpr σ expr
    | isNormalForm expr = expr
    | isδredex expr    = δreduce expr
    | otherwise         = case expr of
    Variable x                     -> lookupEnv x σ
    Add e1 e2 | isRedex e1         -> Add (reduceExpr σ e1 ) e2
              | otherwise          -> Add e1 (reduceExpr σ e2)
    Multiply e1 e2 | isRedex e1    -> Multiply (reduceExpr σ e1) e2
                   | otherwise     -> Multiply e1 (reduceExpr σ e2)
    LessThan e1 e2 | isRedex e1    -> LessThan (reduceExpr σ e1) e2
                   | otherwise     -> LessThan e1 (reduceExpr σ e2)
    And e1 e2 | isRedex e1         -> And (reduceExpr σ e1) e2
              | otherwise          -> And e1 (reduceExpr σ e2)
    Or e1 e2 | isRedex e1          -> Or (reduceExpr σ e1) e2
             | otherwise           -> Or e1 (reduceExpr σ e2)
    Not e                          -> Not (reduceExpr σ e)
    _                              -> error "Unknown expression"

reduceSequence :: (Ord a, Show a) => Env Expr a -> Expr a -> [Expr a]
reduceSequence = (till isNormalForm .) . iterate . reduceExpr

till :: (a -> Bool) -> [a] -> [a]
till p xs = case break p xs of
  (ys,[])  -> ys
  (ys,z:_) -> ys ++ [z]

-- |
-- displays reduce sequence.
-- 
-- >>> runExprMachine env0 exp0
-- (1 × 2) ＋ (3 × 4)
-- 2 ＋ (3 × 4)
-- 2 ＋ 12
-- 14
-- >>> let exp4 = LessThan (Number 5) (Add (Number 2) (Number 2))
-- >>> runExprMachine env0 exp4
-- 5 ＜ (2 ＋ 2)
-- 5 ＜ 4
-- False
-- >>> let exp5 = Add (Variable "x") (Variable "y")
-- >>> let env5 = fromListEnv [("x",Number 3),("y",Number 4)]
-- >>> runExprMachine env5 exp5
-- x ＋ y
-- 3 ＋ y
-- 3 ＋ 4
-- 7
runExprMachine :: (Ord a, Show a) => Env Expr a -> Expr a -> IO ()
runExprMachine = (mapM_ (putStrLn . render . pprExpr) .) . reduceSequence

isReducible :: Stm a -> Bool
isReducible DoNothing = False
isReducible _         = True

reduceStm :: (Ord a, Show a) => Stm a -> Env Expr a -> (Stm a, Env Expr a)
reduceStm stm σ
    | not . isReducible $ stm = (stm,σ)
    | otherwise               = case stm of
    Assign x e 
        | isNormalForm e -> (DoNothing, insertEnv x e σ)
        | otherwise      -> (Assign x (reduceExpr σ e), σ)
    If e t f
        | isNormalForm e -> case e of
            Boolean c -> (if c then t else f, σ)
            _         -> error "Type error"
        | otherwise      -> (If (reduceExpr σ e) t f, σ)
    While e s
        | isNormalForm e -> case e of Boolean c -> (if c then Sequence s stm else DoNothing, σ)
        | otherwise      -> (While (reduceExpr σ e) s, σ)
    _ -> error (render (pprStm stm))
