{-# LANGUAGE PolyKinds #-}
-- |

module Ondim.MultiState where
import Control.Monad.Trans.MultiState.Strict
import Data.HList.ContainsType (ContainsType (setHListElem, getHListElem))
import Prelude hiding (All)

mGets :: MonadMultiGet t m => (t -> b) -> m b
mGets f = do
    s <- mGet
    return (f s)
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

{- | This function works like @withReaderT@, in the sense that it creates a new
   scope for the state in which state changes do not leak outside.
-}
withMultiStateT :: (Monad m, ContainsType t ts) =>
  (t -> t) -> MultiStateT ts m a -> MultiStateT ts m a
withMultiStateT f st =
  MultiStateT $ StateT \s -> (, s) <$>
    runMultiStateTA (setHListElem (f $ getHListElem s) s) st
{-# INLINE withMultiStateT #-}

{- | @All p as@ ensures that the constraint @p@ is satisfied by all the 'types' in
   @as@.
-}
type family All (p :: k -> Constraint) (as :: [k]) :: Constraint where
  All p '[]       = ()
  All p (a ': as) = (p a, All p as)
