{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Tapi.Dal
  ( DAL (..)
  , 
  ) where

import           Data.Semigroup (Option)
import     qualified      Tapi.Models   as M
import Data.Void (Void)
import Control.Exception (try, throw)
import GHC.IO.Exception (IOException(IOError))
import Control.Monad (void)
import Foreign.C (throwErrnoIf)
import qualified Control.Exception as E

type ID = Integer;
class DAL m a | m -> a where
  type family CreationAttributes a
  type family UpdateAttributesT a

  create ::  
    m -> 
    Maybe (CreationAttributes a) -> 
    Maybe (M.CreateOptions m) -> 
    Either M.Error Void

  get ::
    m ->
    Integer ->
    Maybe (M.FindOptions m) ->
    Either M.Error Void

instance M.Models m a => DAL m a where
  type CreationAttributes a = a
  type UpdateAttributesT a = a

  create = M.create

  get m id ops = 
    let result = M.findByPk m Nothing (Just M.FindOptions {
      -- Here, omitted where options
      --  whereOps = Just M.WhereAttributeHash  
      whereOps = Nothing
      , ..
      -- .^ symbol (..), all field labels are brought into scope
    }) in
    case result of
      Left errT -> error $ M.throwErrMsg errT
      _ -> result