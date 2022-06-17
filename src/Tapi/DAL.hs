{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Tapi.Dal
  () where

import           Data.Semigroup (Option)
import           Tapi.Models    (CreateOptions, FindOptions,
                                 ModelOptions (ModelOptions),
                                 Models (initModel), SaveOptions)

type ID = Integer;

data DALMethod
  = DALMethod {
      create'    :: forall c m. c -> Maybe (CreateOptions m)
    , createAny' :: forall c opt m. c -> Maybe (CreateOptions opt) -> m
  }

data Nil;

class DAL m a where
  type family CreationAttributes a
  type family UpdateAttributesT a

  create :: CreationAttributes a -> a -> Maybe (CreateOptions opt) -> m
  update :: m -> UpdateAttributesT a -> a -> SaveOptions m -> m

-- "To obtain the Models after the arrow, we need the Models before the arrow.”
instance Models moM moC => DAL moM moC where
  type CreationAttributes moC = moC
  type UpdateAttributesT moC = moC

  -- create t t2 = do
    -- initModel t "" ModelOptions {}
  update = update
