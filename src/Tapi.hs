{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Tapi where

-- import Tapi.Models
--     ( createModel,
--       Models,
--       ModelReturnT,
--       ModelOptions(..),
--       ModelsT,
--       ColumnOptions, Error
--     )
-- import Control.Arrow (Arrow(first, second))

-- data WaitlistAttributes = WaitlistAttributes {
--     personId :: Integer
--   , name :: String
--   , email :: String
--   , phone :: String
-- }
-- data WaitlistCreationAttributes = WaitlistCreationAttributes
--   String String String

-- applyWaitlistModel :: ModelsT WaitlistAttributes WaitlistCreationAttributes
-- applyWaitlistModel  = do
--   let attr = WaitlistAttributes {
--     personId = 2,
--     name = "",
--     email = "",
--     phone = ""
--   }

--   let options  =  ModelOptions {
--     omitNullModelOpt = False
--     , timestamps = False
--     , paranoid = False
--   }

--   createModel attr "Waitlist" options

-- TODO:
-- mkArchitect

someFunc :: IO ()
someFunc = do
  -- mkArchitect
  putStrLn "Hello Haskell"
