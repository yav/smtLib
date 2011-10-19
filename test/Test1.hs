{-# LANGUAGE OverloadedStrings #-}
import SMTLib1.QF_AUFBV as BV
import System.Process
import System.IO

main :: IO ()
main =
  do let txt = show (pp script)
     putStrLn txt
     putStrLn (replicate 80 '-')
     -- putStrLn =<< readProcess "yices" ["-smt", "-tc"] txt
     putStrLn =<< readProcess "yices" ["-f"] txt


script :: Script
script = Script "Test"
  [ logic "QF_AUFBV"
  , constDef "a" (tBitVec 8)
  , goal (BV.concat (bv 1 8) (bv 3 8) === bv 259 16)
  ]


script1 :: Script
script1 = Script "Test"
  [ logic "QF_BV"
  , constDef "x" (tBitVec 8)
  , assume (c "x" === bv 0 8)
  , goal (c "x" ===  bv 256 8)
  ]

c x = App x []

