module Language.SIMPLE.Environment where

import Data.Map (Map)
import qualified Data.Map as M

type Env f a = Map a (f a)

insertEnv :: (Ord a) => a -> f a -> Env f a -> Env f a
insertEnv = M.insert

lookupEnv :: (Ord a) => a -> Env f a -> f a
lookupEnv = (maybe (error "undefined variable") id .) . M.lookup

fromListEnv :: (Ord a) => [(a, f a)] -> Env f a
fromListEnv = M.fromList

toListEnv :: (Ord a) => Env f a -> [(a, f a)]
toListEnv = M.toList
