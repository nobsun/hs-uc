{-# LANGUAGE TupleSections #-}
-- | 
-- Pretty Printer for SIMPLE Language.
module Language.SIMPLE.PrettyPrint (pprExpr,pprStm,pprEnv,pprStepStm) where

import Control.Arrow ((***))
import Data.List
import Text.PrettyPrint
import Language.SIMPLE.AbstractSyntax
import Language.SIMPLE.Environment

-- $setup
-- >>> type Name = String

-- | 
-- Pretty Printer for Expression
-- 
-- >>> putStr $ render $ pprExpr (Number 7 :: Expr Name)
-- 7
-- >>> putStr $ render $ pprExpr (Boolean True :: Expr Name)
-- True
-- >>> putStr $ render $ pprExpr (Variable "x" :: Expr Name)
-- x
-- >>> putStr $ render $ pprExpr (Add (Number 2) (Add (Number 3) (Number 4)) :: Expr Name)
-- 2 ＋ (3 ＋ 4)
-- >>> putStr $ render $ pprExpr (Multiply (Add (Number 2) (Number 3)) (Number 4) :: Expr Name)
-- (2 ＋ 3) × 4
-- >>> putStr $ render $ pprExpr (LessThan (Number 2) (Number 3) :: Expr Name)
-- 2 ＜ 3
-- >>> putStr $ render $ pprExpr (And (LessThan (Number 2) (Number 3)) (LessThan (Number 4) (Number 3)) :: Expr Name)
-- (2 ＜ 3) ∧ (4 ＜ 3)
-- >>> putStr $ render $ pprExpr (Or (LessThan (Number 2) (Number 3)) (LessThan (Number 4) (Number 3)) :: Expr Name)
-- (2 ＜ 3) ∨ (4 ＜ 3)
-- >>> putStr $ render $ pprExpr (Not (LessThan (Number 4) (Number 3)) :: Expr Name)
-- ¬(4 ＜ 3)
pprExpr :: Show a => Expr a -> Doc
pprExpr expr = case expr of
    Number n       -> integer . toInteger $ n
    Boolean b      -> text . show $ b
    Variable x     -> text . filter ('\"'/=) . show $ x
    Add e1 e2      -> pprExpr' e1 <+> text "＋" <+> pprExpr' e2
    Multiply e1 e2 -> pprExpr' e1 <+> text "×" <+> pprExpr' e2
    LessThan e1 e2 -> pprExpr' e1 <+> text "＜" <+> pprExpr' e2
    And e1 e2      -> pprExpr' e1 <+> text "∧" <+> pprExpr' e2
    Or e1 e2       -> pprExpr' e1 <+> text "∨" <+> pprExpr' e2
    Not e          -> text "¬" <> pprExpr' e

pprExpr' :: Show a => Expr a -> Doc
pprExpr' expr | isAtom expr  = pprExpr expr
              | otherwise    = parens (pprExpr expr)

-- |
-- Pretty Printer for Statement
--
-- >>> putStr $ render $ pprStm (DoNothing :: Stm Name)
-- 
-- >>> putStr $ render $ pprStm (Assign "z" (Add (Variable "z") (Number 1)) :: Stm Name)
-- z := z ＋ 1
-- >>> putStr $ render $ pprStm (If (LessThan (Variable "y") (Number 0)) (Assign "z" (Add (Variable "y") (Number 1))) (Assign "z" (Variable "y")) :: Stm Name)
-- if (y ＜ 0) z := y ＋ 1 else z := y
-- >>> putStr $ render $ pprStm (Sequence (Assign "z" (Add (Variable "z") (Number 1))) (Assign "w" (Variable "z")) :: Stm Name)
-- z := z ＋ 1; w := z
-- >>> putStr $ render $ pprStm (While (LessThan (Variable "i") (Number 10)) (Sequence (Assign "sum" (Add (Variable "sum") (Variable "i"))) (Assign "i" (Add (Variable "i") (Number 1)))) :: Stm Name)
-- while (i ＜ 10) {sum := sum ＋ i; i := i ＋ 1}
pprStm :: Show a => Stm a -> Doc
pprStm stm = case stm of
  DoNothing              -> empty
  Assign x e             -> text (filter ('\"'/=) $ show x) <+> text ":=" <+> pprExpr e
  If e s1 s2             -> text "if" <+> parens (pprExpr e) <+> pprStm' s1 <+> text "else" <+> pprStm' s2
  Sequence DoNothing s2  -> pprStm s2
  Sequence s1 DoNothing  -> pprStm s1
  Sequence s1 s2         -> pprStm s1 <> text ";" <+> pprStm s2
  While e s              -> text "while" <+> parens (pprExpr e) <+> pprStm' s

pprStm' :: Show a => Stm a -> Doc
pprStm' stm | isCompound stm = braces . pprStm $ stm
            | otherwise      = pprStm $ stm

-- |
-- Pretty Printer for Environment.
--
-- >>> putStr $ render $ pprEnv $ (fromListEnv [("x",Number 3),("y",Add (Variable "x") (Number 5))] :: Env Expr Name)
-- {x <- 3; y <- x ＋ 5}
pprEnv :: (Ord a, Show a) => Env Expr a -> Doc
pprEnv = braces . hcat . intersperse (semi <> space) . map pprBinding . toListEnv
  where
    pprBinding (x,v) = text (dequote (show x)) <+> text "<-" <+> pprExpr v
    dequote = filter ('\"'/=)

-- |
-- Pretty Printer for both Statement and Environment
--
-- >>> putStr $ render $ pprStepStm (While (LessThan (Variable "i") (Number 10)) (Sequence (Assign "sum" (Add (Variable "sum") (Variable "i"))) (Assign "i" (Add (Variable "i") (Number 1)))) :: Stm Name,fromListEnv [("sum",Number 0),("i",Number 0)])
-- while (i ＜ 10) {sum := sum ＋ i; i := i ＋ 1} {i <- 0; sum <- 0}
pprStepStm :: (Ord a, Show a) => (Stm a, Env Expr a) -> Doc
pprStepStm = uncurry (<+>) . (pprStm *** pprEnv)
