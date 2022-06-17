
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecordWildCards #-}

module Tapi.Models
  ( createModel
  , ModelsT
  , Models(..)
  , ModelReturnT(..)
  , ModelOptions(..)
  , CreateOptions (..)
  , FindOptions (..)
  , SaveOptions (..)
  , ColumnOptions (..)
  , WhereOptions(..)
  , Error(..)
) where

import           Prelude        hiding (id, init)

import           Data.Semigroup (Option)
import           Data.Void      (Void)
import           Tapi.Utils     (Generic, RecordAccessor, getRecord, (:=))
import Data.Data (Typeable)

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

data ModelReturnT m
  = ModelReturnT ColumnOptions
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
type instance GetArg m ModelOptions = ModelReturnT m;

data Error
  = DatabaseError 
  | NilValue
  deriving (Show, Typeable)

data Identifier
  = IdentifierStr String
  | IdentifierNum Integer
  | IdentifierV Void

-- | The interface for Models
type ModelsT a b
  = (Models a b) => ModelReturnT a

class Models (m :: *) (a :: *) | m -> a where
  type family CreationAttributes a
  type family UpdateAttributesT a

  -- Return the initialized model
  initModel ::
    m
    -> ModelName
    -> ModelOptions
    -> ModelReturnT m
  --
  -- Manage model
  manageModel :: 
    m -> Manager m
  --
  -- Builds a new model instance and calls save on it
  create ::
    m
    -> Maybe a
    -> Maybe ops
    -> Either Error Void
  -- 
  -- Search for a single instance by its primary key
  findByPk ::
    m
    -> Maybe Identifier
    -> Maybe ops
    -> Either Error Void
  -- 
  -- ...

-- | Scale up in future!
instance (Models m c, Monad ModelReturnT) => Models m c where
  type CreationAttributes c = c;
  type UpdateAttributesT c = c;

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
      ModelOptions {..} -> ModelReturnT {};
    }
    return'

  manageModel = manageModel
  create = create
  findByPk = findByPk


-- | Synonym for `init`
createModel :: (Models m a) =>
  m
  -> ModelName
  -> ModelOptions
  -> ModelReturnT m
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
data CreateOptions a
  = CreateOptions {
      fieldsCreateOpt   :: [a]
    , ignoreDuplicates  :: Bool
    , returning         :: CreateOptionsReturning a
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

data WhereOptions a
  = WhereAttributeHash 
  | Where
  | Json 

-- | Representation for Options that are passed to any model creating a SELECT query
data FindOptions a
  = FindOptions {
      include:: Includeable
    , order   :: Order
    , limit   :: Integer
    , offset  :: Integer
    , whereOps ::  Maybe (WhereOptions a)
  }

data SaveOptions a
  = SaveOptions {
      fieldsSaveOpt   :: [a]
    , validateSaveOpt :: Bool
    , omitNullSaveOpt :: Bool
  }
