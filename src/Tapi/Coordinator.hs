{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapi.Coordinator where

import qualified Tapi.Models as M
import qualified Tapi.Dal as D

import Data.Void (Void)

class CreateCoordinator m a c | m -> a where
  type family DAL m a c

  create ::
    m
    -> c
    -> transact
    -> Void 

instance D.DAL m a => CreateCoordinator m a c where
  type DAL m a c = a
  
-- type family ListElems a = b | b -> a
-- type instance ListElems [a] = a

-- class Concat a b where
--   type ConcatTy a b
--   trans :: a -> b -> ConcatTy a b
--   reves :: ConcatTy a b -> b -> a

-- instance Num String => Concat Int String where
--   type ConcatTy Int String = Bool
--   trans x y = False
--   reves a b = 2