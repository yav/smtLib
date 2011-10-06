{-# LANGUAGE OverloadedStrings #-}
import SMTLib1
import SMTLib1.QF_BV
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
  [ logic "QF_BV"
  , constDef "x" (tBitVec 8)
  , assume (c "x" === bv 0 8)
  , goal (c "x" ===  bv 256 8)
  ]

c x = App x []

