{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Tapi (someFunc)
import Tapi.Models

-- APPLIED
-- 
data WaitlistAttributes = WaitlistAttributes {
    personId :: Integer
  , name :: String
  , email :: String
  , phone :: String
}

data WaitlistCreationAttributes = WaitlistCreationAttributes 
  String String String

applyWaitlistModel :: (Models WaitlistAttributes WaitlistCreationAttributes) => 
   ModelCtor WaitlistAttributes
applyWaitlistModel  = do
  let attr = WaitlistAttributes {
    personId = 2,
    name = "",
    email = "",
    phone = ""
  } 

  let options  =  ModelOptions {
    omitNull = False
    , timestamps = False
    , paranoid = False
  }

  createModel attr "Waitlist" options


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Tapi.someFunc
