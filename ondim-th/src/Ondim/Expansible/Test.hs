{-# LANGUAGE TemplateHaskell #-}

module Ondim.Expansible.Test where

import Ondim.Expansible.TH

data family Lol

data Foo = FooMe Text Foo | FooYou Bar
  deriving (Show)

data Bar = BarMe Foo | BarYou Text
  deriving (Show)

$(makeHoles ''Foo [''Text])

deriving instance (Show a) => Show (Foo' a)
