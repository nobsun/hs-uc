module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     $ map ("src/Language/SIMPLE/"++)
     ["AbstractSyntax.hs"
     ,"Environment.hs"
     ,"TransitionSemantics.hs"
     ,"PrettyPrint.hs"
     ]
