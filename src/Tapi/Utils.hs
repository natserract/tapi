{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Tapi.Utils (
  Generic
) where
import Data.Kind (Constraint)


type Generic i o = forall x. i x -> o x
