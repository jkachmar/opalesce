module Main where

import           Api    (app)
import           Config (Config (..), Environment (..), makePool)

main :: IO ()
main = putStrLn "Hello"
