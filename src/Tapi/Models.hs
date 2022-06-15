
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Tapi.Models 
  ( 
    createModel,
    setModelOptions,
    
    ModelCtor(..),
    ModelOptions(..),

    -- Exports the class, the associated type Models and the member functions 
    Models(..)
  ) where

import Data.Void ( Void )
import Prelude hiding (id, init)
import Data.Data (Proxy)

import Control.Monad

data ColumnOptions = ColumnOptions {
    allowNull :: Bool
  , field :: String
  , defaultValue :: Void
}

data ModelOptions = ModelOptions {
    omitNull :: Bool
  , timestamps :: Bool
  , paranoid :: Bool
}

data ModelAttributes a = ModelAttributes {
    primaryKey :: Bool
  , values :: [String]
  , getDataValue :: a -> Proxy a
}

data ModelCtor m = ModelCtor (ModelAttributes m) ColumnOptions

class Models m a | m -> a where
  -- Return the initialized model
  init :: 
    m 
    -> String 
    -> ModelOptions 
    -> ModelCtor m
  -- 
  -- Set model options
  setOptions :: m -> ModelOptions

type ModelName = String;
    
-- | Synonym for `init`
createModel :: (Models a b) => 
  a 
  -> ModelName 
  -> ModelOptions
  -> ModelCtor a
createModel = init

setModelOptions :: (Models a b) => a -> ModelOptions 
setModelOptions = setOptions
