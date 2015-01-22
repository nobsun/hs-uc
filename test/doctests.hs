module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     $ map ("src/Language/SIMPLE/"++)
     ["Environment.hs"
     ,"TransitionSemantics.hs"
     ,"PrettyPrint.hs"
     ]
