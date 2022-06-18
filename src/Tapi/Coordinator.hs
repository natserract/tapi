{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards #-}

module Tapi.Coordinator
  ( CreateCoordinator(..)
  , ReadCoordinator(..)
  )where

import qualified Tapi.DAL    as D
import qualified Tapi.Models as M

import           Data.Void   (Void)
import           Tapi.Models (CommonOptions (transaction), CreateOptions)

class CreateCoordinator m a | m -> a where
  type CreationAttributes a

  create ::
    m
    -> CreationAttributes a
    -> Maybe M.Transaction
    -> Either M.Error Void

instance D.DAL m a => CreateCoordinator m a where
  type CreationAttributes a = a

  create mod params transact =
      D.create mod (Just params) (
        Just M.CommonOptions {
          transaction = transact
        },
        Just M.CreateOptions {}
      )

data PaginationParams
  = PaginationParams {
    limit  :: Int,
    offset :: Int
  }

class ReadCoordinator m a | m -> a where
  type ReadingAttributes a

  getPlain ::
    m
    -> Integer
    -> Maybe M.Transaction
    -> Either M.Error Void

  get ::
    m
    -> Integer
    -> Maybe M.Transaction
    -> Either M.Error Void

  -- ...another implementation here

instance D.DAL m a => ReadCoordinator m a where
  type ReadingAttributes a = a

  getPlain mod id transact =
    let result = D.get mod id (Just M.CommonOptions {
      transaction = transact
    }, Nothing) in

    case result of
      Left M.NilValue -> error "cannot find model in `getPlain` method!"
      Left err        ->  error $ M.throwErrMsg err
      _               -> result


  get mod id transact =
    let result = D.get mod id (Just M.CommonOptions {
      transaction = transact
    }, Just M.FindOptions {
      include = M.Includeable {
        all = True
        , ..
      }
    }) in

    case result of
      Left M.NilValue -> error "cannot find model in `get` method!"
      Left err        ->  error $ M.throwErrMsg err
      _               -> result
