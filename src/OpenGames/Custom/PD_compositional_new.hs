{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module OpenGames.Custom.PD_compositional_new where

import Control.Monad
import Control.Lens
import Control.Monad.State

import OpenGames.Custom.RLGeneral hiding (sample)

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef

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

-- this lens just inserts the updated value into the map
-- in all functions, i is a key of type (state, action)
update :: (Ord state)  => MonadicLens IO (IORef (Map (state, Action) Double)) () ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double)
update = monadicLens @IO read write
  where
    read ref (s, a) = do
      m <- readIORef ref
      let dist = createGreedyProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a') m)
                | a' <-  [Testify, StaySilent]]
      pure (Map.findWithDefault 0 (s, a) m, dist)

    write ref (i, x) =
      modifyIORef' ref (Map.insert i x)

-- f is of type (i -> Double), (i, x) is of type (i, Double)
-- f gets the current value at index i and sums it with x, the output of the previous step (the bellman target)
-- this completes the Q update function
learningRate :: Double -> MonadicLens IO ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double) ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double)
learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {(y, dist) <- f i; pure (i, alpha*x + (1 - alpha)*y)}


-- this computes the Q-Learning target based on (s, a, r, s)
-- f is a function that gets the q-value at a specific array index, j is the array index of the maximum possible state-action pair for a given state, i is the array index of the state-action pair to update 
-- x is the reward
bellman :: Double -> MonadicLens IO ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double) ((state, Action) -> IO (Double, [(Action, Double)])) (state, Action, Double, state)
bellman discountFactor = monadicLens @IO id $ \f (s, a, r, s') -> do {(y, dist) <- f (s', a); pure ((s, a), r + discountFactor*y)}

backwardMap :: (Ord state)  => Double -> Double -> MonadicLens IO (IORef (Map (state, Action) Double)) () ((state, Action) -> IO (Double, [(Action, Double)])) (state, Action, Double, state)
-- update . learningRate alpha . bellman gamma is equal to the block G' in the string diagram of rl cybernetics
backwardMap alpha gamma =
    update
  . learningRate alpha
  . bellman gamma

-- agent :: MonadicLens IO state (state, Action, Double, state) action (state, reward)

    

-- this lens takes a function f as its argument, which should return the next state and payoff, given the current state and the chosen action
-- the forward pass of this lens is from (state, action) to (), the backward pass is from () to (state, Double)
-- essentially, the forward pass does nothing and the backward pass uses f to return the next state and current payoff
environment :: (state -> Action -> IO (state, Double)) -> MonadicLens IO (state, Action) (state, Double) () ()
environment f = monadicLens @IO (const ()) (const . uncurry f)

pdRewards :: state -> Action -> IO (state, Double)
pdRewards s a = pure (s, prisonersDilemmaMatrix a Testify)

epsilonGreedy :: ((state, Action) -> IO Double) -- ^ Function to get Q-values
              -> Double                         -- ^ Epsilon (probability of random action)
              -> state                           -- ^ Current state
              -> IO Action                       -- ^ Chosen action
epsilonGreedy getStateActionVal epsilon s = do
    rand <- randomRIO (0.0, 1.0)
    let allActions = [Testify, StaySilent]  -- All possible actions
    if rand < epsilon
        then do
            -- Pick a random action
            idx <- randomRIO (0, length allActions - 1)
            return $ allActions !! idx
        else do
            -- Pick the greedy action
            actionValues <- forM allActions $ \a -> do
                v <- getStateActionVal (s, a)
                return (a, v)
            let (bestAction, _) = maximumBy (comparing snd) actionValues
            return bestAction
    

readQ :: (Ord state) => IORef (Map (state, Action) Double) -> (state, Action) -> IO Double
readQ ref i = do
      m <- readIORef ref
      pure $ Map.findWithDefault 0 i m

qStep :: IO ()
qStep = do
    initialQTable <- newIORef $  Map.fromList [((0, StaySilent), 0), ((0, Testify), 0)]
    let initialState = 0
    
    agentAction <- epsilonGreedy (readQ initialQTable) 0.1 initialState

    

    return ()
