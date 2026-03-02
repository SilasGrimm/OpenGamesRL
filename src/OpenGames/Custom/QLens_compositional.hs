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

