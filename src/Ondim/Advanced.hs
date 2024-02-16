{- | This module is for advanced users. Generally speaking, you should only use
   this module if you want to define new targets (formats) or customize existing
   ones.
-}
module Ondim.Advanced
  ( OndimNode (..),

    -- * Combinators
    module Ondim.MultiWalk.Combinators,

    expandSpecList,

    -- * Substructures
    module Ondim.MultiWalk.Substructure,
  ) where

import Ondim.MultiWalk.Class (OndimNode (..))
import Ondim.MultiWalk.Combinators
import Ondim.MultiWalk.Substructure
import Ondim.MultiWalk.Core (expandSpecList)
