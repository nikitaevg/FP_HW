module Main where

import Program (runProgram)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let name = head args
    text <- readFile name
    runProgram name text
