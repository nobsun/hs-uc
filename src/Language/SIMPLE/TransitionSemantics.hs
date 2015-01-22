-- | This module provides transition semantics for the SIMPLE language
module Language.SIMPLE.TransitionSemantics (
   isRedex
  ,reduceExpr
  ,reduceSequenceOfExpr
  ,runExprMachine
  ,isReducible
  ,reduceStm
  ,reduceSequenceOfStm
  ,runMachine
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

-- |
-- makes expression reduce sequence.
reduceSequenceOfExpr :: (Ord a, Show a) => Env Expr a -> Expr a -> [Expr a]
reduceSequenceOfExpr = (till isNormalForm .) . iterate . reduceExpr

till :: (a -> Bool) -> [a] -> [a]
till p xs = case break p xs of
  (ys,[])  -> ys
  (ys,z:_) -> ys ++ [z]

-- |
-- displays expression reduce sequence.
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
runExprMachine = (mapM_ (putStrLn . render . pprExpr) .) . reduceSequenceOfExpr

-- |
-- Predicate whether the specified statement is reducible.
isReducible :: Stm a -> Bool
isReducible DoNothing = False
isReducible _         = True

-- |
-- make a small-step reducing of the specified statement in the specified environment.
--
-- >>> let stm1 = Assign "x" (Add (Variable "x") (Number 1))
-- >>> putStr $ render $ pprStm stm1
-- x := x ＋ 1
-- >>> let env1 = fromListEnv [("x", Number 2)]
-- >>> putStr $ render $ pprEnv env1
-- {x <- 2}
-- >>> isReducible stm1
-- True
-- >>> let step2@(env2,stm2) = reduceStm env1 stm1
-- >>> putStr $ render $ pprStepStm step2
-- x := 2 ＋ 1 {x <- 2}
-- >>> let step3@(env3,stm3) = reduceStm env2 stm2
-- >>> putStr $ render $ pprStepStm step3
-- x := 3 {x <- 2}
-- >>> let step4@(env4,stm4) = reduceStm env3 stm3
-- >>> putStr $ render $ pprStepStm step4
-- φ {x <- 3}
-- >>> isReducible stm4
-- False
reduceStm :: (Ord a, Show a) => Env Expr a -> Stm a -> (Env Expr a, Stm a)
reduceStm σ stm
    | not . isReducible $ stm = (σ,stm)
    | otherwise               = case stm of
    Assign x e 
        | isNormalForm e -> (insertEnv x e σ, DoNothing)
        | otherwise      -> (σ, Assign x (reduceExpr σ e))
    If e t f
        | isNormalForm e -> case e of
            Boolean c -> (σ, if c then t else f)
            _         -> error "Type error"
        | otherwise      -> (σ, If (reduceExpr σ e) t f)
    While e s -> (σ,If e (Sequence s stm) DoNothing)
    Sequence DoNothing s2 -> reduceStm σ s2
    Sequence s1 s2 -> case reduceStm σ s1 of
        (σ',s1')    -> (σ' ,Sequence s1' s2)
    _ -> error (render (pprStm stm))

-- |
-- makes statement reduce sequence.
reduceSequenceOfStm :: (Ord a, Show a) => Env Expr a -> Stm a -> [(Env Expr a, Stm a)]
reduceSequenceOfStm σ stm = till (not . isReducible . snd) $ iterate (uncurry reduceStm) (σ,stm)

-- |
-- displays statement reduce sequence.
--
-- >>> let stm1 = Assign "x" (Add (Variable "x") (Number 1))
-- >>> let env1 = fromListEnv [("x", Number 2)]
-- >>> runMachine env1 stm1
-- x := x ＋ 1 {x <- 2}
-- x := 2 ＋ 1 {x <- 2}
-- x := 3 {x <- 2}
-- φ {x <- 3}
-- >>> let stm2 = If (Variable "x") (Assign "y" (Number 1)) (Assign "y" (Number 2))
-- >>> let env2 = fromListEnv [("x",Boolean True)]
-- >>> runMachine env2 stm2
-- if (x) y := 1 else y := 2 {x <- True}
-- if (True) y := 1 else y := 2 {x <- True}
-- y := 1 {x <- True}
-- φ {x <- True; y <- 1}
-- >>> let stm3 = If (Variable "x") (Assign "y" (Number 1)) DoNothing
-- >>> let env3 = fromListEnv [("x",Boolean False)]
-- >>> runMachine env3 stm3
-- if (x) y := 1 else φ {x <- False}
-- if (False) y := 1 else φ {x <- False}
-- φ {x <- False}
-- >>> let stm4 = Sequence (Assign "x" (Add (Number 1) (Number 1))) (Assign "y" (Add (Variable "x") (Number 3)))
-- >>> runMachine env0 stm4
-- x := 1 ＋ 1; y := x ＋ 3 {}
-- x := 2; y := x ＋ 3 {}
-- y := x ＋ 3 {x <- 2}
-- y := 2 ＋ 3 {x <- 2}
-- y := 5 {x <- 2}
-- φ {x <- 2; y <- 5}
-- >>> let stm5 = While (LessThan (Variable "x") (Number 5)) (Assign "x" (Multiply (Variable "x") (Number 3))) :: Stm Name
-- >>> let env5 = fromListEnv [("x",Number 1)] :: Env Expr Name
-- >>> runMachine env5 stm5
-- while (x ＜ 5) x := x × 3 {x <- 1}
-- if (x ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 1}
-- if (1 ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 1}
-- if (True) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 1}
-- x := x × 3; while (x ＜ 5) x := x × 3 {x <- 1}
-- x := 1 × 3; while (x ＜ 5) x := x × 3 {x <- 1}
-- x := 3; while (x ＜ 5) x := x × 3 {x <- 1}
-- while (x ＜ 5) x := x × 3 {x <- 3}
-- if (x ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 3}
-- if (3 ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 3}
-- if (True) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 3}
-- x := x × 3; while (x ＜ 5) x := x × 3 {x <- 3}
-- x := 3 × 3; while (x ＜ 5) x := x × 3 {x <- 3}
-- x := 9; while (x ＜ 5) x := x × 3 {x <- 3}
-- while (x ＜ 5) x := x × 3 {x <- 9}
-- if (x ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 9}
-- if (9 ＜ 5) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 9}
-- if (False) {x := x × 3; while (x ＜ 5) x := x × 3} else φ {x <- 9}
-- φ {x <- 9}
runMachine :: (Ord a, Show a) => Env Expr a -> Stm a -> IO ()
runMachine = (mapM_ (putStrLn . render . pprStepStm) .) . reduceSequenceOfStm
