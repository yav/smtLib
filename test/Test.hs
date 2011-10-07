{-# LANGUAGE OverloadedStrings #-}
import SMTLib1.QF_AUFBV
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
  [ logic "QF_AUFBV"
  , constDef "a" (tArray 4 7)
  , constDef "i" (tBitVec 4)
  , constDef "v" (tBitVec 7)
  , assume (c "v" === bv 6 7)
  , assume (select (c "a") (c "i") === bv 6 7)
  , goal   (select (c "a") (c "i") === c "v")
  ]


script1 :: Script
script1 = Script "Test"
  [ logic "QF_BV"
  , constDef "x" (tBitVec 8)
  , assume (c "x" === bv 0 8)
  , goal (c "x" ===  bv 256 8)
  ]

c x = App x []

