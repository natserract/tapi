{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Tapi (someFunc) where

import Tapi.Models
    ( createModel,
      Models,
      ModelCtor,
      ModelOptions(
          ModelOptions
        , omitNull
        , timestamps
        , paranoid),
      ModelsT 
    )

data WaitlistAttributes = WaitlistAttributes {
    personId :: Integer
  , name :: String
  , email :: String
  , phone :: String
}

data WaitlistCreationAttributes = WaitlistCreationAttributes 
  String String String

applyWaitlistModel :: ModelsT WaitlistAttributes WaitlistCreationAttributes
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

someFunc :: IO ()
someFunc = 
  putStrLn "Hello Haskell"
