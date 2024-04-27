{-# LANGUAGE RoleAnnotations #-}
module Ondim.Internal.Basic where

type role Ondim nominal nominal
data Ondim (s :: Type) (a :: Type)

instance Applicative (Ondim s)
instance Monad (Ondim s)
