{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Tapi (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Tapi.someFunc
