
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Tapi.Models
  ( createModel
  , setModelOptions
  , Models(..)
  , ModelCtor(..)
  , ModelOptions(..)
  , ModelsT
  , CreateOptions(..)
  , FindOptions (..)
) where

import Prelude hiding (id, init)

import Tapi.Utils (Generic, (:=))
import Data.Semigroup (Option)

data ColumnOptions
  = ColumnOptions {
      allowNull :: Bool
    , field :: String
    , defaultValue :: ()
  }

data ModelOptions m
  = ModelOptions {
      omitNull :: Bool
    , timestamps :: Bool
    , paranoid :: Bool
    , primaryKey :: Bool
    , values :: [String]
  }

instance (Show (ModelOptions m)) where
  show v = unlines 
    [ "Options {"
     , " omitNull         = " ++ show (omitNull v)
     , " timestamps       = " ++ show (timestamps v)
     , " paranoid         = " ++ show (paranoid v)
     , " primaryKey       = " ++ show (primaryKey v)
     , " values           = " ++ show (values v)
     , "}"
    ]

-- | Show all model options
showOptions :: ModelOptions a -> String
showOptions = show

data ModelCtor m
  = ModelCtor ColumnOptions
  | Unkown

type ModelName = String;

type family GetArg m o;
type instance GetArg m (Option a) = m;
type instance GetArg m (ModelOptions m) = ModelCtor m;

class Models (m :: *) (c :: *) | m -> c where
  type family Values c;
  
  -- Return the initialized model
  init ::
    m
    -> ModelName
    -> ModelOptions m
    -> ModelCtor m
  -- 
  -- Set model options
  setOptions :: a -> ModelOptions m
  -- 
  -- Returns all values of the instance
  -- getValues :: GetArg m ModelOptions;
  -- 
  -- Set value
  -- toJSON

instance (Models a b, Monad ModelCtor) => Models a b where
  type Values b = b;

  init modelAtrr modelName modelOpt = do
    let return' = init modelAtrr modelName modelOpt
    case modelOpt of {
      ModelOptions {
        omitNull = False
      } ->
        -- Do some action here!
        -- Depend on options
        -- ...
        return'
      ;
      ModelOptions {} -> ModelCtor {};
    }
    return'

  setOptions = setOptions
  -- getValues = getValues;


-- | Synonym for `init`
createModel :: (Models a b, Monad ModelCtor) =>
  a
  -> ModelName
  -> ModelOptions a
  -> ModelCtor a
createModel = init

-- | Synonym for `setOptions`
setModelOptions :: (Models a b, Monad ModelCtor) => a -> ModelOptions a
setModelOptions = setOptions

-- | The interface for Models
type ModelsT a b 
  = (Models a b, Monad ModelCtor) => ModelCtor a

-- 
-- | Options
-- 
type family CreateOptionsReturning k
type instance CreateOptionsReturning Bool = Bool
type instance CreateOptionsReturning [ss] = [ss]

-- | Representation for Model.create options method
data CreateOptions opt
  = CreateOptions {
      fields :: [opt]
    , ignoreDuplicates :: Bool
    , returning :: CreateOptionsReturning opt
    , validate :: (:=) Bool
    -- Dont' confuse ^ is just a synonym of Bool :: * (one kind)
  }
data Order
  = Fn
  | Col
  | Literal
  | OrderItems

data Includeable
  = Includeable {
      all :: Bool
    , nested :: Maybe Bool 
  }

-- | Representation for Options that are passed to any model creating a SELECT query
data FindOptions opt
  = FindOptions {
      include:: Includeable
    , order :: Order
    , limit :: Integer
    , offset :: Integer
  }
