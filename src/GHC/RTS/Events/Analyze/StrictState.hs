{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | State monad which forces the state to whnf on every step
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, CPP #-}
module GHC.RTS.Events.Analyze.StrictState (
    -- * Transformer
    StateT
  , runStateT
  , evalStateT
  , execStateT
    -- * Base monad
  , State
  , runState
  , evalState
  , execState
  , modify
    -- * Re-exports
  , module Control.Monad.State.Strict
  ) where

import Control.Lens
import Control.Lens.Internal.Zoom
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState(..))
import qualified Control.Monad.State.Strict as St
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Identity (Identity(..))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

{-------------------------------------------------------------------------------
  Transformer
-------------------------------------------------------------------------------}

newtype StateT s m a = StateT { unStateT :: St.StateT s m a  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type instance Zoomed (StateT s m) = Focusing m
instance Monad m => Zoom (StateT s m) (StateT t m) s t where zoom l = StateT . zoom l . unStateT

runStateT :: StateT s m a -> s -> m (a, s)
runStateT = St.runStateT . unStateT

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT = St.evalStateT . unStateT

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT = St.execStateT . unStateT

instance Monad m => MonadState s (StateT s m) where
  get   = StateT get
  put s = s `seq` StateT (put s)

{-------------------------------------------------------------------------------
  Base monad
-------------------------------------------------------------------------------}

type State s a = StateT s Identity a

runState :: State s a -> s -> (a, s)
runState act = runIdentity . runStateT act

evalState :: State s a -> s -> a
evalState act = runIdentity . evalStateT act

execState :: State s a -> s -> s
execState act = runIdentity . execStateT act

modify :: (s -> s) -> State s ()
modify = StateT . St.modify'
