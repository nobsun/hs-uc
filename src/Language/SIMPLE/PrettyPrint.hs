{-# LANGUAGE TupleSections #-}
module Language.SIMPLE.PrettyPrint where

import Control.Arrow ((***))
import Data.List
import Text.PrettyPrint
import Language.SIMPLE.AbstractSyntax
import Language.SIMPLE.Environment

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

pprEnv :: (Ord a, Show a) => Env Expr a -> Doc
pprEnv = braces . hcat . intersperse (semi <> space) . map pprBinding . toListEnv
  where
    pprBinding (x,v) = text (dequote (show x)) <+> text "<-" <+> pprExpr v
    dequote = filter ('\"'/=)

pprStepStm :: (Ord a, Show a) => (Stm a, Env Expr a) -> Doc
pprStepStm = uncurry (<+>) . (pprStm *** pprEnv)
