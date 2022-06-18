
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UndecidableInstances   #-}

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
  , Transaction(..)
  , throwErrMsg
  , CommonOptions(..)
  , BlockOptions
  , BlockOptionsTupl
  , Includeable(..)
  , accessFirstBlockOptions
  , accessSecondBlockOptions
) where

import           Prelude        hiding (id, init)

import           Data.Data      (Typeable)
import           Data.Semigroup (Option)
import           Data.Void      (Void)
import           Tapi.Utils     (Generic, RecordAccessor, getRecord, (:=))

data ColumnOptions
  = ColumnOptions {
      allowNull    :: Bool
    , field        :: String
    , defaultValue :: ()
  }

-- | Get column options
getColumnOptions ::
  ColumnOptions ->
  field ->
  RecordAccessor ColumnOptions field
getColumnOptions = getRecord
--
-- ~ Type level
--
type family GetColumnOptions
  (c :: ColumnOptions)
  (field) :: RecordAccessor ColumnOptions field where
  -- ...

data ModelOptions
  = ModelOptions {
      omitNullModelOpt :: Bool
    , timestamps       :: Bool
    , paranoid         :: Bool
    , primaryKey       :: Bool
    , values           :: [String]
  }
instance (Show ModelOptions) where
  show v = unlines
    [ "Options {"
     , " omitNullModelOpt         = " ++ show (omitNullModelOpt v)
     , " timestamps               = " ++ show (timestamps v)
     , " paranoid                 = " ++ show (paranoid v)
     , " primaryKey               = " ++ show (primaryKey v)
     , " values                   = " ++ show (values v)
     , "}"
    ]

-- | Get model options
getModelOptions ::
  ModelOptions ->
  field ->
  RecordAccessor ModelOptions field
getModelOptions = getRecord

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
  | RequestValidationError
  | NilValue
  deriving (Show, Typeable)

-- | Throwing error based on `M.Error`
throwErrMsg :: Error -> [Char]
throwErrMsg err = case err of
  DatabaseError          -> "Database Error!"
  RequestValidationError -> "Request Validation Error!"
  NilValue               -> "Object is Nil!"

data Identifier
  = IdentifierStr String
  | IdentifierNum Integer
  | IdentifierV Void

-- | The interface for Models
type ModelsT m a
  = (Models m a) => ModelReturnT m

class Models (m :: *) (a :: *) |
  m -> a
  -- `m -> a` states that `a` type is determined by `m`
  -- For any given m you can only have one `a`.
  -- `a` should not be `a` free variable, it is determined by the `m` variables
  --
  where

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
    -> ops
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
manageOptions :: (Models m a) => m -> Manager m
manageOptions = manageModel

--
-- | Options
-- ~ Open type families
type family CreateOptionsReturning k
type instance CreateOptionsReturning Bool = Bool
type instance CreateOptionsReturning [ss] = [ss]

data Transaction
  = READ_UNCOMMITTED
  | READ_COMMITTED
  | REPEATABLE_READ
  | SERIALIZABLE
  | DEFERRED
  | IMMEDIATE
  | EXCLUSIVE

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

data WhereOptions
  = WhereAttributeHash
  | Where
  | Json

-- | Representation for Options that are passed to any model creating a SELECT query
data FindOptions
  = FindOptions {
      include  :: Includeable
    , order    :: Order
    , limit    :: Integer
    , offset   :: Integer
    , whereOps ::  Maybe WhereOptions
  }

data SaveOptions a
  = SaveOptions {
      fieldsSaveOpt   :: [a]
    , validateSaveOpt :: Bool
    , omitNullSaveOpt :: Bool
  }

newtype CommonOptions
  = CommonOptions {
    transaction :: Maybe Transaction
  }

-- | Type with multiple record constructor
-- ~ sum type strategy
--
type BlockOptions ops
  = Either CommonOptions ops

-- Problems:
{--When we pass this ex: `
  get mod id $ Left M.CommonOptions {
    transaction = transact
  }`

  The question here, how we put another options, e.g from `FindOptions`

  So, i think we can use *tuple values* strategy
  (CommonOptions, *Options)

  F: get mod id $ (CommonOptions, FindOptions)

  Access:
    *1 (fst ops) ~ CommonOptions -> get first value
    *2 (snd ops) ~ FindOptions -> get second
--}

type BlockOptionsTupl o = (Maybe CommonOptions, Maybe o)

accessFirstBlockOptions :: BlockOptionsTupl o -> Maybe CommonOptions
accessFirstBlockOptions = fst -- (x, y) -> x

accessSecondBlockOptions :: BlockOptionsTupl o -> Maybe o
accessSecondBlockOptions = snd -- (x, y) -> y
