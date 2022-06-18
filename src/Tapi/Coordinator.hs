{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Tapi.Coordinator where

import qualified Tapi.Dal    as D
import qualified Tapi.Models as M

import           Data.Void   (Void)
import           Tapi.Models (CreateOptions (transaction))

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
      D.create mod (Just params) $ Just M.CreateOptions {
        transaction = transact
        , ..
      }
