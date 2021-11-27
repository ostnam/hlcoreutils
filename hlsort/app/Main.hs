module Main where

import Lib

main :: IO ()
main = do
  lines <- getAllLines
  let linesTree = Node lines (Nil) (Nil)
  let sortedLinesTree = sortString linesTree
  printStrs $ concatTriTree sortedLinesTree
