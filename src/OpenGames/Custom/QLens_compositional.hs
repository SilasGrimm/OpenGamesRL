{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Custom.QLens_compositional where

import Control.Monad
import Control.Monad.State hiding (State)
-- import Control.Lens

import Control.Monad.Reader
import Control.Monad.Writer

import Data.Functor.Identity
import Data.Functor.Const

import OpenGames.Custom.RLGeneral hiding (sample)

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.IORef

-- import OpenGames.Custom.PrisonersDilemma_new

import OpenGames.Custom.RLGeneral

import System.Random

import Data.Map (Map)
import qualified Data.Map as Map
import OpenGames.Custom.RLGeneral (Prob)


-- Lens implementation

-- class RightModule m f where
--   act :: f (m a) -> f a

-- instance (Monad m) => RightModule m m where
--   act = join
 
-- instance RightModule m (Const a) where
--   act = Const . getConst

-- type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

-- monadicLens :: forall m f s t a b . (Functor f, RightModule m f) 
--   => (s -> a) -> (s -> b -> m t) -> LensLike f s t a b
-- monadicLens v u k s = act (fmap (u s) (k (v s)))

-- -- this lens just inserts the updated value into the map
-- -- in all functions, i is a key of type (state, action)
-- update :: Ord i => MonadicLens IO (IORef (Map i Double)) () (i -> IO Double) (i, Double)
-- update = monadicLens @IO read write
--   where
--     read ref i = do
--       m <- readIORef ref
--       pure $ Map.findWithDefault 0 i m

--     write ref (i, x) =
--       modifyIORef' ref (Map.insert i x)

-- -- f is of type (i -> Double), (i, x) is of type (i, Double)
-- -- f gets the current value at index i and sums it with x, the output of the previous step (the bellman target)
-- -- this completes the Q update function
-- learningRate :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double)
-- learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {y <- f i; pure (i, alpha*x + (1 - alpha)*y)}


-- -- this computes the Q-Learning target based on (s, a, r, s)
-- -- f is a function that gets the q-value at a specific array index, j is the array index of the maximum possible state-action pair for a given state, i is the array index of the state-action pair to update 
-- -- x is the reward
-- bellman :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double, i)
-- bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do {y <- f j; pure (i, x + discountFactor*y)}

-- backwardMap :: Ord i => Double -> Double -> MonadicLens IO (IORef (Map i Double)) () (i -> IO Double) (i, Double, i)
-- -- update . learningRate alpha . bellman gamma is equal to the block G' in the string diagram of rl cybernetics
-- backwardMap alpha gamma =
--     update
--   . learningRate alpha
--   . bellman gamma


-- this lens just inserts the updated value into the map
-- in all functions, i is a key of type (state, action)
-- update :: (Ord state)  => MonadicLens IO (IORef (Map (state, Action) Double)) () ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double)
-- update = monadicLens @IO read write
--   where
--     read ref (s, a) = do
--       m <- readIORef ref
--       let dist = createGreedyProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a') m)
--                 | a' <-  [Testify, StaySilent]]
--       pure (Map.findWithDefault 0 (s, a) m, dist)

--     write ref (i, x) =
--       modifyIORef' ref (Map.insert i x)

-- -- f is of type (i -> Double), (i, x) is of type (i, Double)
-- -- f gets the current value at index i and sums it with x, the output of the previous step (the bellman target)
-- -- this completes the Q update function
-- learningRate :: Double -> MonadicLens IO ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double) ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double)
-- learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {(y, dist) <- f i; pure (i, alpha*x + (1 - alpha)*y)}


-- -- this computes the Q-Learning target based on (s, a, r, s)
-- -- f is a function that gets the q-value at a specific array index, j is the array index of the maximum possible state-action pair for a given state, i is the array index of the state-action pair to update 
-- -- x is the reward
-- bellman :: Double -> MonadicLens IO ((state, Action) -> IO (Double, [(Action, Double)])) ((state, Action), Double) ((state, Action) -> IO (Double, [(Action, Double)])) (state, Action, Double, state)
-- bellman discountFactor = monadicLens @IO id $ \f (s, a, r, s') -> do {(y, dist) <- f (s', a); pure ((s, a), r + discountFactor*y)}

-- qLens_compositional :: (Ord state)  => Double -> Double -> MonadicLens IO (IORef (Map (state, Action) Double)) () ((state, Action) -> IO (Double, [(Action, Double)])) (state, Action, Double, state)
-- -- update . learningRate alpha . bellman gamma is equal to the block G' in the string diagram of rl cybernetics
-- qLens_compositional alpha gamma =
--     update
--   . learningRate alpha
--   . bellman gamma

-- -- | Runs one step of Q-Learning using the compositional lens.
-- -- We invoke the lens with a continuation that simply "injects" the experience tuple.
-- runQStep :: (Ord state) 
--          => IORef (Map (state, Action) Double)    -- ^ The Q-Table State
--          -> (state, Action, Double, state)        -- ^ The experience (s, a, r, s')
--          -> IO ()
-- runQStep ref experience = 
--     qLens_compositional 0.1 0.9 (\_reader -> pure experience) ref

type CustomLens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- usage example: over (customQLens 0.1 0.8 (const [])) (\qFocus -> (0, OpenGames.Custom.PrisonersDilemma_new.Testify, 2, 0)) OpenGames.Custom.PrisonersDilemma_new.initialQTable 
over :: CustomLens s t a b -> (a -> b) -> s -> t
over lens f s = runIdentity (lens (Identity . f) s)


-- usage example: view (customQLens 0.1 0.8 (const [])) OpenGames.Custom.PrisonersDilemma_new.initialQTable
view :: CustomLens s t a b -> s -> a
view lens s = getConst (lens Const s)

customLens :: (s -> a) -> (s -> b -> t) -> CustomLens s t a b
customLens getter setter = \f s -> setter s <$> f (getter s)

customQLensUpdate :: (Ord state, Ord action) => CustomLens (QTable state action) (QTable state action) ((state, action) -> Double) ((state, action), Double)
customQLensUpdate = customLens
    (\q i -> Map.findWithDefault 0 i q)
    (\q (i, x) -> Map.insert i x q)

customQLensLearningRate :: (Ord state, Ord action) => Double -> CustomLens ((state, action) -> Double) ((state, action), Double) ((state, action) -> Double) ((state, action), Double)
customQLensLearningRate alpha = customLens
    id
    (\qFocus (sa, target) -> let currentQValue = qFocus sa in (sa, (1 - alpha) * currentQValue + alpha * target))

customQLensBellmanTarget :: (Ord state, Ord action) => Double -> Double -> (state -> [action]) ->  CustomLens ((state, action) -> Double) ((state, action), Double) (state -> [(action, Double)]) (state, action, Double, Maybe state)
customQLensBellmanTarget gamma epsilon getActions = customLens
    -- id
    (\qFocus s -> createProbabilitiesFromRewards [ (a, qFocus (s, a)) | a <- getActions s ] epsilon) -- returns dist
    compute
    where
      compute qFocus (s, a, r, Nothing) = ((s, a), r)
      compute qFocus (s, a, r, Just s') = let y = getArgmax s' qFocus in ((s, a), r + gamma * y)


      getArgmax s qFocus = maximum [qFocus (s, a) | a <- getActions s]

-- note that composition is now backwards, i.e. the type of customQLens without the extra  arguments is 
  -- ((state -> [(action, Double)]) -> f (state, action, Double, Maybe state)) -> QTable state action -> f (QTable state action)
-- or equivalently
  -- CustomLens (QTable state action) (QTable state action) (state -> [(action, Double)]))  (state, action, Double, Maybe state)
customQLens alpha gamma epsilon getActions = customQLensUpdate . customQLensLearningRate alpha . customQLensBellmanTarget gamma epsilon getActions



customQLensBellmanTargetSoftmax :: (Ord state, Ord action) => Double -> (state -> [action]) ->  CustomLens ((state, action) -> Double) ((state, action), Double) (state -> [(action, Double)]) (state, action, Double, Maybe state)
customQLensBellmanTargetSoftmax gamma getActions = customLens
    -- id
    (\qFocus s -> let norm = sum [exp (qFocus (s, a)) | a <- getActions s] in [ (a, exp (qFocus (s, a)) / norm) | a <- getActions s ]) -- returns dist
    compute
    where
      compute qFocus (s, a, r, Nothing) = ((s, a), r)
      compute qFocus (s, a, r, Just s') = let y = getArgmax s' qFocus in ((s, a), r + gamma * y)


      getArgmax s qFocus = maximum [qFocus (s, a) | a <- getActions s]

customQLensSoftmax alpha gamma getActions = customQLensUpdate . customQLensLearningRate alpha . customQLensBellmanTargetSoftmax gamma getActions

