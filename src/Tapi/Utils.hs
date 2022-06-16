{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tapi.Utils 
  ( Generic
  , All
  , (:=)
  ) where

import Data.Kind (Constraint)

-- (* -> *) -> (* -> *) -> *
type Generic i o = forall x. i x -> o x

-- *
data All = All

-- * -> *
infixl 4 :=
data (:=) sh deriving (Eq, Show)
instance Num ((:=) m) where
  (+) = (+)
  (*) = (*)
  abs = abs
  signum = signum
  fromInteger = fromInteger
  negate = negate
  