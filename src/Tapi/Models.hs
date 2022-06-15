
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapi.Models
  (
    createModel,
    setModelOptions,

    ModelCtor(..),
    ModelOptions(..),
    ModelsT,

    -- Exports the class, the associated type Models and the member functions 
    Models(..)
  ) where

import Data.Void ( Void )
import Prelude hiding (id, init)
import Data.Data (Proxy)

import Tapi.Utils (Generic)

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

data ModelCtor m =  ModelCtor (ModelAttributes m)  ColumnOptions

type ModelName = String;

class Models (m :: *) (c :: *) | m -> c where
  -- Return the initialized model
  init ::
    m
    -> ModelName
    -> ModelOptions
    -> ModelCtor m
  -- 
  -- Set model options
  setOptions :: m -> ModelOptions

instance (Models a b, Monad ModelCtor) => Models a b where
  init modelAtrr modelName modelOpt = do
    let return' = init modelAtrr modelName modelOpt
    case modelOpt of { 
      ModelOptions True _ _ ->
        -- Do some action here!
        -- ...
        return'
      ;
      ModelOptions {} -> ModelCtor {};
    }
    return'

  setOptions = setOptions

-- | Synonym for `init`
createModel :: (Models a b, Monad ModelCtor) =>
  a
  -> ModelName
  -> ModelOptions
  -> ModelCtor a
createModel = init

-- | Synonym for `setOptions`
setModelOptions :: (Models a b, Monad ModelCtor) => a -> ModelOptions
setModelOptions = setOptions

-- | Reusable, generic `Models` type 
type ModelsT a b = 
  (Models a b, Monad ModelCtor) => ModelCtor a