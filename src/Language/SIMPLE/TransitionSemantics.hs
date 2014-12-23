module Language.SIMPLE.TransitionSemantics where

import Control.Applicative
import Text.PrettyPrint
import Language.SIMPLE.AbstractSyntax
import Language.SIMPLE.Environment
import Language.SIMPLE.PrettyPrint


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
