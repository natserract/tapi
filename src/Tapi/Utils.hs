{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Tapi.Utils
  ( Generic
  , Nil
  , (:=)
  , RecordAccessor (..)
  , getRecord
  ) where

import           Data.Kind (Constraint)
import           GHC.Base  (Nat, Symbol)

-- (* -> *) -> (* -> *) -> *
type Generic i o = forall x. i x -> o x

-- *
data Nil

-- * -> *
infixl 4 :=
data (:=) sh deriving (Eq, Show, Num)

-- | Acceess record field
type RecordAccessor
  (r :: *) a = r -> a -> a

-- | Get the value of a field.
{--
  recordS = RecordName { fieldName = value, ...}
  getRecord record field

  type :: ^ RecordAccessor RecordT (RecordT -> FieldT)
-}
getRecord ::
  r ->
  a ->
  RecordAccessor r a
getRecord = getRecord

-- | Compared candidate type
isSameType :: (a ~ b) => a -> b -> a
isSameType = isSameType
--
-- ~ Type level
-- # Closed type family
--
-- IsSameType Int Int ~ Int
-- IsSameType String Int ~ String
--
type family IsSameType (ρ :: k) (τ :: k) :: * where
  IsSameType a a = a
  IsSameType a b = a
