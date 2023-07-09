{-# OPTIONS_GHC -Wno-orphans #-}

module Ondim.MultiWalk.Combinators where

import {-# SOURCE #-} Ondim.MultiWalk.Class

instance (OndimNode a) => OndimNode [a]
