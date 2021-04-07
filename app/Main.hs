module Main (main) where

import Hforth (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
