{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Tapi.Dal
  ( DAL (..)
  ,
  ) where

import           Data.Semigroup (Option)
import           Data.Void      (Void)
import qualified Tapi.Models    as M

type ID = Integer;

class DAL m a | m -> a where
  create ::
    m ->
    Maybe a ->
    Maybe (M.CreateOptions m) ->
    Either M.Error Void

  get ::
    m ->
    Integer ->
    Maybe (M.FindOptions m) ->
    Either M.Error Void

  -- TODO:
  -- createAny
  -- where
  -- first
  -- update
  -- updateAny
  -- delete
  -- count

instance M.Models m a => DAL m a where
  --
  create = M.create

  --
  get m id ops =
    let result = M.findByPk m Nothing (Just M.FindOptions {
      -- Here, omitted where options
       whereOps = Just M.WhereAttributeHash
      -- whereOps = Nothing
      , ..
      -- .^ symbol (..), all field labels are brought into scope
    }) in
    case result of
      Left errT -> error $ M.throwErrMsg errT
      _         -> result

  -- ...another implementation here
