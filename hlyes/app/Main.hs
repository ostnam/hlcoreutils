module Main where

import Lib
import System.Environment (getArgs)
import System.Exit
import GHC.Base

main :: IO ()
main = do
  args <- getArgs

  when (isHelp args) printHelp
  when (isVersion args) printVersion
  when (isVersion args || isHelp args) exitSuccess

  yes args
