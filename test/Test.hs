{-# LANGUAGE OverloadedStrings #-}
import SMTLib1
import System.Process
import System.IO

main :: IO ()
main =
  do let txt = show (pp script)
     putStrLn txt
     putStrLn (replicate 80 '-')
     putStrLn =<< readProcess "yices" ["-smt", "-tc"] txt

script :: Script
script = Script "Test"
  [ CmdExtraFuns [ FunDecl "f" [] "Int" [] ]
  , CmdAssumption $ FPred "=" [ App "f" [], 0 ]
  , CmdFormula $ FPred "=" [ App "f" [], 0 ]
  ]

