module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Language/SIMPLE/AbstractSyntax.hs"]
