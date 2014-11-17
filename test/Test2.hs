{-# LANGUAGE OverloadedStrings #-}
import SMTLib2
import SMTLib2.Core
import SMTLib2.BitVector
import System.Process
import System.IO

main :: IO ()
main =
  do let txt = show (pp script)
     putStrLn txt
     putStrLn (replicate 80 '-')
     putStrLn =<< readProcess "stp" ["--SMTLIB2"] txt


script :: Script
script = Script
  [ CmdSetLogic "QF_BV"
  , CmdDeclareFun "x" [] (tBitVec 4)
  , CmdAssert (c "x" === bv 3 4)
  , CmdCheckSat
  , CmdComment "hooray"
  , CmdExit
  ]

c x = app x []
