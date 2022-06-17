{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE KindSignatures    #-}

module Tapi.Utils
  ( Generic
  , All
  , (:=)
  , RecordAccessor (..)
  , getRecord
  ) where

import           Data.Kind (Constraint)

-- (* -> *) -> (* -> *) -> *
type Generic i o = forall x. i x -> o x

-- *
data All = All

-- * -> *
infixl 4 :=
data (:=) sh deriving (Eq, Show, Num)

-- | Acceess record field
type RecordAccessor (r :: *) a = r -> a -> a

-- | Get the value of a field.
{--
  recordS = RecordName { fieldName = value, ...}
  getRecord record field

  type :: ^ RecordAccessor RecordT (RecordT -> FieldT)
-}
getRecord :: r -> a -> RecordAccessor r a
getRecord = getRecord
