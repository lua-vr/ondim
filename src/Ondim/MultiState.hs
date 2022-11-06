{-# LANGUAGE PolyKinds #-}
-- |

module Ondim.MultiState where
import Control.Monad.Trans.MultiState.Strict
import Prelude hiding (All)

mGets :: MonadMultiGet t m => (t -> b) -> m b
mGets f = f <$> mGet
{-# INLINE mGets #-}

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mState (\s -> ((), f s))
{-# INLINE mModify #-}

mState :: MonadMultiState s m => (s -> (a, s)) -> m a
mState f = do
  s <- mGet
  let ~(a, s') = f s
  mSet s'
  return a
{-# INLINE mState #-}

