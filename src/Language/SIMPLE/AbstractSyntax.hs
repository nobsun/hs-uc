-- |
-- Abstract Syntax for SIMPLE Languguage
module Language.SIMPLE.AbstractSyntax (
     Expr(..)
    ,isAtom
    ,Stm(..)
    ,isCompound
    ) where

-- |
-- Expression
--
data Expr a = Number Int
            | Boolean Bool
            | Variable a
            | Add (Expr a) (Expr a)     
            | Multiply (Expr a) (Expr a)
            | And (Expr a) (Expr a)     
            | Or (Expr a) (Expr a)      
            | Not (Expr a)              
            | LessThan (Expr a) (Expr a)
            deriving (Show)

-- |
-- Predicate whether the specified expression is atomic or not.
--
isAtom :: Expr a -> Bool
isAtom e = case e of
    Number _    -> True
    Boolean _   -> True
    Variable _  -> True
    _           -> False

-- |
-- Statement
--
data Stm a = DoNothing
           | Assign a (Expr a)
           | If (Expr a) (Stm a) (Stm a)
           | Sequence (Stm a) (Stm a)
           | While (Expr a) (Stm a)
           deriving (Show)

-- |
-- Predicate whether the specified statement is compound or not.
--
isCompound :: Stm a -> Bool
isCompound stm = case stm of
    Sequence _ _ -> True
    _            -> False
