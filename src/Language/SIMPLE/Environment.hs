-- | This module provides a environment for SIMPLE language.
module Language.SIMPLE.Environment (
   Env
  ,emptyEnv
  ,insertEnv
  ,lookupEnv
  ,unionEnv
  ,fromListEnv
  ,toListEnv) where

import Data.Map (Map)
import qualified Data.Map as M

-- $setup
-- >>> import Language.SIMPLE.AbstractSyntax
-- >>> import Language.SIMPLE.PrettyPrint
-- >>> import Text.PrettyPrint

-- |
-- Environment
type Env f a = Map a (f a)

-- |
-- Empty environment
emptyEnv :: Env f a
emptyEnv = M.empty

-- |
-- inserts an entry into the specified environment.
--
-- >>> putStr $ render $ pprEnv $ insertEnv "x" (Number 0) emptyEnv
-- {x <- 0}
insertEnv :: (Ord a) => a -> f a -> Env f a -> Env f a
insertEnv = M.insert

-- |
-- unions two environments
--
-- >>> let oldEnv = fromListEnv [("y",Number 5)]
-- >>> putStr $ render $ pprEnv oldEnv
-- {y <- 5}
-- >>> let newEnv = unionEnv oldEnv (fromListEnv [("x",Number 3)])
-- >>> putStr $ render $ pprEnv newEnv
-- {x <- 3; y <- 5}
-- >>> putStr $ render $ pprEnv oldEnv
-- {y <- 5}
unionEnv :: (Ord a) => Env f a -> Env f a -> Env f a
unionEnv = M.union

-- |
-- lookups value of the variable in specified environment.
--
-- >>> putStr $ render $ pprExpr $ lookupEnv "x" $ insertEnv "x" (Number 0) emptyEnv
-- 0
-- >>> putStr $ render $ pprExpr $ lookupEnv "x" emptyEnv
-- *** Exception: undefined variable
lookupEnv :: (Ord a) => a -> Env f a -> f a
lookupEnv = (maybe (error "undefined variable") id .) . M.lookup

-- |
-- constructs an environment from the associated a-list.
--
-- >>> putStr $ render $ pprEnv $ fromListEnv [("x",Number 3),("y",Add (Variable "x") (Number 1))]
-- {x <- 3; y <- x ＋ 1}
fromListEnv :: (Ord a) => [(a, f a)] -> Env f a
fromListEnv = M.fromList

-- |
-- converts an environment to the associated a-list.
--
-- >>> toListEnv $ fromListEnv [("x",Number 3),("y",Add (Variable "x") (Number 1))]
-- [("x",Number 3),("y",Add (Variable "x") (Number 1))]
toListEnv :: (Ord a) => Env f a -> [(a, f a)]
toListEnv = M.toList
