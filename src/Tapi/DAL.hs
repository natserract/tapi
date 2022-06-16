{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Tapi.Dal 
  () where

import Tapi.Models (CreateOptions, FindOptions, Models)
import Data.Semigroup (Option)

type ID = Integer;

data DALMethod 
  = DALMethod {
      create' :: forall c m. c -> Maybe (CreateOptions m)
    , createAny' ::  forall c opt m. c -> Maybe (CreateOptions opt) -> m
  } 

class DAL m c u | m -> c, m -> u where
  type family DalModelCtor m
  type family DalValues c

  create :: DalValues c -> Maybe (CreateOptions m)
  createAny :: DalValues c -> Maybe (CreateOptions opt) -> m
  whereF :: FindOptions m -> [m]
  get :: ID -> Maybe (FindOptions m) -> Maybe m
  first :: FindOptions m -> m
  update :: m -> u -> FindOptions m -> m
  updateAny :: m -> u -> m
