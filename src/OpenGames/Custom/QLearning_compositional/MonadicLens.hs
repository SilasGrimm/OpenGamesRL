{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module OpenGames.Custom.QLearning_compositional.MonadicLens where

import Control.Monad
import Control.Lens
import Control.Monad.State

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map

import OpenGames.Custom.RLGeneral hiding (sample)

import Data.Ix
import Data.Array.MArray
import Data.Array.IO
import Data.Map (Map)
import qualified Data.Map as Map
import OpenGames.Custom.PrisonersDilemma_new

import System.Random

class RightModule m f where
  act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
  act = join
 
instance RightModule m (Const a) where
  act = Const . getConst

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicLens :: forall m f s t a b . (Functor f, RightModule m f) 
  => (s -> a) -> (s -> b -> m t) -> LensLike f s t a b
monadicLens v u k s = act (fmap (u s) (k (v s)))

-- this lens just inserts the updated value into the array
update :: (Ix i) => MonadicLens IO (IOUArray i Double) () (i -> IO Double) (i, Double)
update = monadicLens @IO readArray (uncurry . writeArray)

-- f is of type (i -> Double), (i, x) is of type (i, Double)
-- f gets the current value at index i and sums it with x, the output of the previous step (bellman)
-- this completes the Q update function
learningRate :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double)
learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {y <- f i; pure (i, alpha*x + (1 - alpha)*y)}


-- this computes the Q-Learning target based on (s, a, r, s)
-- f is a function that gets the q-value at a specific array index, j is the array index of the maximum possible state-action pair for a given state, i is the array index of the state-action pair to update 
-- x is the reward
bellman :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double, i)
bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do {y <- f j; pure (i, x + discountFactor*y)}


-- update . learningRate alpha . bellman gamma is equal to the block G' in the string diagram of rl cybernetics

-- policy that chooses an action should also be implemented as a lens

-- model :: MonadicLens IO (IOUArray i Double) () [(action, Prob)] (state, action, Double, state)
-- model = monadicLens @IO createProbabilitiesFromRewards


-- we need one more layer in between here to convert from state-action pairs to indices in the Q-Table

sample :: (state, [(action, Prob)]) -> IO action
sample (_, xs) = do
  r <- randomRIO (0, 1)
  pure (go r xs)
  where
    go _ [] = error "empty distribution"
    go p ((a,w):rest)
      | p <= w    = a
      | otherwise = go (p - w) rest

-- agent :: MonadicLens IO (state, [(action, Prob)]) (state, action, Double, state) (state, action) (action, IO (state, Double))
-- agent = monadicLens @IO (\(s, dist) -> do {a <- sample (s, dist); pure (s, a)}) (\(s, _) (a, s'r) -> do {(s', r) <- s'r; pure (s, a, r, s')})

agent :: MonadicLens IO (state, [(action, Prob)]) (state, action, Double, state) (state, [(action, Prob)]) (action, state, Double)
agent = monadicLens @IO id (\(s, dist) (a, s', r) -> pure (s, a, r, s'))

-- this lens takes a function f as its argument, which should return the next state and payoff, given the current state and the chosen action
-- the forward pass of this lens is from (state, action) to (), the backward pass is from () to (state, Double)
-- essentially, the forward pass does nothing and the backward pass uses f to return the next state and current payoff
environment :: (state -> action -> IO (state, Double)) -> MonadicLens IO (state, action) (state, Double) () ()
environment f = monadicLens @IO (const ()) (const . uncurry f)

-- we sample the action in the environment lens
environment' :: (state -> action -> (state, Double)) -> MonadicLens IO (state, [(action, Prob)]) (action, state, Double) () ()
environment' f = monadicLens @IO (const ()) (\(s, dist) () -> do {a <- sample (s, dist); let (s, r) = f s a in pure (a, s, r)})

-- state is always 0 for prisoners dilemma, opponent always testifies
pdRewards :: state -> Action -> (state, Double)
pdRewards s a = (s, prisonersDilemmaMatrix a Testify)

greedy :: (Enum action) => ((state, action) -> IO Double) -> state -> IO action
greedy = undefined


-------------------------------
-- same logic with Map as QTable

------------------------------------------------------------
-- Map-based Q-table helpers
------------------------------------------------------------

-- Read a Q-value, defaulting to 0 if the key is missing
readMap :: Ord i => IORef (Map i Double) -> i -> IO Double
readMap ref i = do
  m <- readIORef ref
  pure $ Map.findWithDefault 0 i m

-- Write a Q-value
writeMap :: Ord i => IORef (Map i Double) -> (i, Double) -> IO ()
writeMap ref (i, x) =
  modifyIORef' ref (Map.insert i x)

------------------------------------------------------------
-- Lenses rewritten to use Map
------------------------------------------------------------

-- Inserts the updated value into the Map
update'
  :: Ord i
  => MonadicLens
       IO
       (IORef (Map i Double))
       ()
       (i -> IO Double)
       (i, Double)
update' =
  monadicLens @IO
    readMap
    (\ref ix -> writeMap ref ix)

-- Learning rate application
learningRate'
  :: Double
  -> MonadicLens
       IO
       (i -> IO Double)
       (i, Double)
       (i -> IO Double)
       (i, Double)
learningRate' alpha =
  monadicLens @IO id $ \f (i, x) -> do
    y <- f i
    pure (i, alpha * x + (1 - alpha) * y)

-- Bellman target computation
bellman'
  :: Double
  -> MonadicLens
       IO
       (i -> IO Double)
       (i, Double)
       (i -> IO Double)
       (i, Double, i)
bellman' discountFactor =
  monadicLens @IO id $ \f (i, x, j) -> do
    y <- f j
    pure (i, x + discountFactor * y)