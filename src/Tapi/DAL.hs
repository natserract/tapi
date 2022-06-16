{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapi.Dal 
  () where

import Tapi.Models (CreateOptions, FindOptions, Models, SaveOptions)
import Data.Semigroup (Option)

type ID = Integer;

data DALMethod 
  = DALMethod {
      create' :: forall c m. c -> Maybe (CreateOptions m)
    , createAny' ::  forall c opt m. c -> Maybe (CreateOptions opt) -> m
  } 

class DAL m a where
  type family CreationAttributes a
  type family UpdateAttributesT a

  create :: CreationAttributes a -> a -> Maybe (CreateOptions opt) -> m
  update :: m -> UpdateAttributesT a -> a -> SaveOptions m -> m


instance Models moM moC => DAL moM moC where
  type CreationAttributes moC = moC
  type UpdateAttributesT moC = moC

  create = create
  update = update
