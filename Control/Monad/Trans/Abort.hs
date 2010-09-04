-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Abort
-- Copyright   :  (c) Gregory Crosswhite 2010
-- License     :  BSD3
-- Maintainer  :  gcross@phys.washington.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a monad and a monad transformer that allow the
-- user to abort a monadic computation and immediately return a
-- result.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Abort (
    -- * The Abort monad
    Abort,
    runAbort,
    -- * The AbortT monad transformer
    AbortT(..),
    runAbortT,
    -- * Abort operations
    abort
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Functor
import Data.Functor.Identity

-- ---------------------------------------------------------------------------
-- | An abort monad, parametrized by the type @r@ of the value to return.
type Abort r = AbortT r Identity

-- | Execute the abort monad computation and return the resulting value.
runAbort :: Abort r r -- ^ the monadic computation to run
         -> r         -- ^ the result of the computation
runAbort = runIdentity . runAbortT

-- ---------------------------------------------------------------------------
-- | An abort monad transformer parametrized by
--
--   * @r@ - the value that will ultimately be returned; and
--
--   * @m@ - the inner monad.
--
-- The 'AbortT' type wraps a monadic value that is either
--
--   * 'Left' @r@, which indicates that the monadic computation has
--     terminated with result @r@ and so all further steps in the computation
--     should be ignored; or
--
--   * 'Right' @a@, which indicates that the computation is proceding normally
--     and that its current value is @a@.
newtype AbortT r m a = AbortT { unwrapAbortT :: m (Either r a) }

instance Functor m => Functor (AbortT r m) where
    fmap f = AbortT . fmap (either Left (Right . f)) . unwrapAbortT

instance Applicative m => Applicative (AbortT r m) where
    pure = AbortT . fmap Right . pure
    (AbortT m) <*> (AbortT x) = AbortT ((fmap h m) <*> x)
      where
        h (Left g) = const (Left g)
        h (Right f) = either Left (Right . f)

instance Monad m => Monad (AbortT r m) where
    return = AbortT . return . Right
    (AbortT m) >>= f = AbortT $ m >>= either (return . Left) (unwrapAbortT . f)

instance MonadIO m => MonadIO (AbortT r m) where
    liftIO = lift . liftIO

instance MonadTrans (AbortT r) where
    lift = AbortT . (>>= return . Right)

-- | Execute the abort monad computation and return the resulting
--   (monadic) value.
runAbortT :: Monad m
          => AbortT r m r -- ^ the monadic computation to run
          -> m r          -- ^ the (monadic) result of the computation
runAbortT (AbortT m) = m >>= either return return

-- ---------------------------------------------------------------------------
-- | Abort the computation and immediately return a result; all steps
-- in the computation after this monadic computation will be ignored.
--
-- Note that since no further computation is performed after this, there is
-- no way for subsequent computations to access the monadic value, and so it
-- can be assigned an arbitrary type.
abort :: Monad m
      => r            -- ^ the result to return
      -> AbortT r m a -- ^ a monadic value that has the effect of
                      --   terminating the computation and immediately
                      --   returning a value; note that since all
                      --   subsequent steps in the computation will be
                      --   ignored, this monadic value can take an
                      --   arbitrary type since its value will never
                      --   be accessed
abort = AbortT . return . Left
