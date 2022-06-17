
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Tapi.Models
  ( createModel
  -- , setModelOptions
  , Models(..)
  , ModelCtor(..)
  , ModelOptions(..)
  , ModelsT
  , CreateOptions(..)
  , FindOptions (..)
  , SaveOptions
  , ColumnOptions
) where

import           Prelude        hiding (id, init)

import           Data.Semigroup (Option)
import           Data.Void      (Void)
import           Tapi.Utils     (Generic, RecordAccessor, getRecord, (:=))

data ColumnOptions
  = ColumnOptions {
      allowNull    :: Bool
    , field        :: String
    , defaultValue :: ()
  }

type GetColumnOptions fie
  = RecordAccessor ColumnOptions fie

-- | Get column options
getColumnOptions ::
  ColumnOptions ->
  iel ->
  GetColumnOptions
  iel
getColumnOptions = getRecord


data ModelOptions
  = ModelOptions {
      omitNullModelOpt :: Bool
    , timestamps       :: Bool
    , paranoid         :: Bool
    , primaryKey       :: Bool
    , values           :: [String]
  }

type GetModelOptions a = a -> ModelOptions -> a

instance (Show ModelOptions) where
  show v = unlines
    [ "Options {"
     , " omitNullModelOpt         = " ++ show (omitNullModelOpt v)
     , " timestamps       = " ++ show (timestamps v)
     , " paranoid         = " ++ show (paranoid v)
     , " primaryKey       = " ++ show (primaryKey v)
     , " values           = " ++ show (values v)
     , "}"
    ]

-- | Show all model options
showOptions :: ModelOptions -> String
showOptions = show

data ModelCtor m
  = ModelCtor ColumnOptions
  | Unkown

data SetOptions
  = SetOptions {
      raw   :: Bool
    , reset :: Bool
  }

newtype KeyOfAttributes m = KeyOfAttributes m

data Manager m
  = Manager {
      setAttributes :: forall key. key -> SetOptions -> KeyOfAttributes m
    , setOptions'   :: ModelOptions -> ModelOptions
  }

type ModelName = String;

type family GetArg m o;
type instance GetArg m (Option a) = m;
type instance GetArg m ModelOptions = ModelCtor m;

data Identifier
  = IdentifierStr String
  | IdentifierNum Integer
  | IdentifierV Void


-- | The interface for Models
type ModelsT a b
  = (Models a b, Monad ModelCtor) => ModelCtor a
class Models (m :: *) (c :: *) | m -> c where
  type family Values c;

  -- Return the initialized model
  initModel ::
    m
    -> ModelName
    -> ModelOptions
    -> ModelCtor m
  --
  -- Manage model options
  manageModel :: 
    m -> Manager m
  --
  -- Builds a new model instance and calls save on it
  create ::
    m
    -> Maybe c
    -> Maybe ModelOptions
    -> Void
  -- 
  -- Search for a single instance by its primary key
  findByPk ::
    m
    -> Maybe Identifier
    -> Maybe ModelOptions
    -> m

instance (Models a b, Monad ModelCtor) => Models a b where
  type Values b = b;

  initModel modelAtrr modelName modelOpt = do
    let return' = initModel modelAtrr modelName modelOpt
    case modelOpt of {
      ModelOptions {
        omitNullModelOpt = False
      } ->
        -- Do some action here!
        -- Depend on options
        -- ...
        return'
      ;
      ModelOptions {..} -> ModelCtor {};
    }
    return'

  manageModel = manageModel
  create = create
  findByPk = findByPk


-- | Synonym for `init`
createModel :: (Models a b, Monad ModelCtor) =>
  a
  -> ModelName
  -> ModelOptions
  -> ModelCtor a
createModel = initModel

-- | Synonym for `manage`
manageOptions :: (Models a b) => a -> Manager a
manageOptions = manageModel

--
-- | Options
--
type family CreateOptionsReturning k
type instance CreateOptionsReturning Bool = Bool
type instance CreateOptionsReturning [ss] = [ss]

-- | Representation for Model.create options method
data CreateOptions opt
  = CreateOptions {
      fieldsCreateOpt   :: [opt]
    , ignoreDuplicates  :: Bool
    , returning         :: CreateOptionsReturning opt
    , validateCreateOpt :: (:=) Bool
    -- Dont' confuse ^ is just a synonym of Bool :: * (one kind)
  }
data Order
  = Fn
  | Col
  | Literal
  | OrderItems

data Includeable
  = Includeable {
      all    :: Bool
    , nested :: Maybe Bool
  }

-- | Representation for Options that are passed to any model creating a SELECT query
data FindOptions opt
  = FindOptions {
      include:: Includeable
    , order   :: Order
    , limit   :: Integer
    , offset  :: Integer
  }

data SaveOptions opt
  = SaveOptions {
      fieldsSaveOpt   :: [opt]
    , validateSaveOpt :: Bool
    , omitNullSaveOpt :: Bool
  }
