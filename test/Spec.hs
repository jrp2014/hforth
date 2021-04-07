module Main (main) where

import Hforth (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
