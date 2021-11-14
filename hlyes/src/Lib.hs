{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
      isHelp,
      isVersion,
      printHelp,
      printVersion,
      yes,
    ) where

import Data.Text (Text)
import qualified Data.Text as T

isHelp :: [String] -> Bool
isHelp = any (T.isInfixOf "-h") . map (T.toLower . T.pack)

isVersion :: [String] -> Bool
isVersion = any (T.isInfixOf "-v") . map (T.toLower . T.pack)

printHelp :: IO ()
printHelp = putStrLn "Show this help with -h, show version info with -v, no args to print y repeatedly, any string to print this string repeatedly."

printVersion :: IO ()
printVersion = putStrLn "hlcat v0.1"

yes :: [String] -> IO()
yes = yes' . map T.pack

yes' :: [Text] -> IO ()
yes' [] = yes' ["y"] 
yes' ["y"] = do
    putChar 'y'
    putChar '\n'
    yes' ["y"]
yes' xs = do
    putStrLn $ T.unpack $ T.intercalate " " xs
    yes' xs
