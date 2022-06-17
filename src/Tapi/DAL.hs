{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Tapi.Dal
  () where

import           Data.Semigroup (Option)
import     qualified      Tapi.Models   as M
import Data.Void (Void)
-- import Tapi.Models (FindOptions(whereOps, limit), WhereOptions)

type ID = Integer;

data DALMethod
  = DALMethod {
      create'    :: forall c m. c -> Maybe (M.CreateOptions m)
    , createAny' :: forall c opt m. c -> Maybe (M.CreateOptions opt) -> m
  }

data Nil;

class DAL m a | m -> a where
  type family CreationAttributes a
  type family UpdateAttributesT a

  create ::  
    m -> 
    Maybe (CreationAttributes a) -> 
    Maybe (M.CreateOptions m) -> 
    Void

  get ::
    m ->
    Integer ->
    Maybe (M.FindOptions m) ->
    Void

instance M.Models moM mAttr => DAL moM mAttr where
  type CreationAttributes mAttr = mAttr
  type UpdateAttributesT mAttr = mAttr

  create
    = M.create

  get m id ops
    = M.findByPk m Nothing (Just M.FindOptions {
      -- Here, omitted where options
      --  whereOps = Just M.WhereAttributeHash  
      whereOps = Nothing
      , ..
    })
