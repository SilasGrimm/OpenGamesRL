{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenGames.Custom.Automated_QOptimizer_ExtensiveGame where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Custom.PrisonersDilemma_new_comp (Action(..))
import OpenGames.Custom.BoS_comp (ActionBoS(..))
import OpenGames.Custom.QLens_compositional
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import System.Random (randomRIO)

import Numeric.Probability.Distribution

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

-- This implementation learns a Q-Agent for an arbitrary sequential game with two players
-- Always the observing dependentDecision in the game is learned
-- To generalize this to extensive games, it must be made possible to learn games with more than two players
--  This is difficult, since the heterogenous type lists needed for the player strategies need to be given at compile time (i.e. the exact length needs to be known)

createOptimizerLensForExtensiveGameAgent :: 
    (Enum action, Bounded action, Ord action, Enum state, Bounded state, Ord state)
    => [action]
    -> Double
    -> Double
    -> Double
    -> CustomLens (QTable state action) (QTable state action) (state -> [(action, Double)]) (state, action, Double, Maybe state)
createOptimizerLensForExtensiveGameAgent possibleActions alpha gamma epsilon = customQLens alpha gamma epsilon (const possibleActions)

runOptimizerForExtensiveGame :: 
    (Enum action, Bounded action, Ord action, Show action, Enum action', Bounded action', Ord action', Show action')
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action',
         Kleisli Stochastic action' action]
       '[[DiagnosticInfoBayesian () action'],
         [DiagnosticInfoBayesian action' action]]
       ()
       ()
       (action, action', Double)
       ()
    -> Kleisli Stochastic () action'
    -> IO (Kleisli Stochastic action' action)
runOptimizerForExtensiveGame game opponentStrategy = do
    let
        possibleStates :: (Enum action', Bounded action') => [action']
        possibleStates = [minBound .. maxBound]

        possibleActions :: (Enum action, Bounded action) => [action]
        possibleActions = [minBound .. maxBound]

        q = Map.fromList [((s, a), 0) | s <- possibleStates, a <- possibleActions]

    learnedQ <- learnStrategy game q (createOptimizerLensForExtensiveGameAgent possibleActions 0.1 0.95 0.1) possibleActions opponentStrategy
    putStrLn $ show (Map.toList learnedQ)

    pure $ strategyFromLens learnedQ (createOptimizerLensForExtensiveGameAgent possibleActions 0.1 0.95 0.0)

trainSteps = 200

learnStrategy :: 
    (Show action, Show action')
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action',
         Kleisli Stochastic action' action]
       '[[DiagnosticInfoBayesian () action'],
         [DiagnosticInfoBayesian action' action]]
       ()
       ()
       (action, action', Double)
       ()
    -> QTable action' action 
    -> CustomLens (QTable action' action) (QTable action' action) (action' -> [(action, Double)]) (action', action, Double, Maybe action')
    -> [action]
    -> Kleisli Stochastic () action'
    -> IO (QTable action' action)
learnStrategy game q lens possibleActions opponentStrategy = do
  learningStep game q lens possibleActions opponentStrategy trainSteps

-- plays n games and returns learned strategy
learningStep :: 
    (Show action, Show action')
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action',
         Kleisli Stochastic action' action]
       '[[DiagnosticInfoBayesian () action'],
         [DiagnosticInfoBayesian action' action]]
       ()
       ()
       (action, action', Double)
       ()
    -> QTable action' action 
    -> CustomLens (QTable action' action) (QTable action' action) (action' -> [(action, Double)]) (action', action, Double, Maybe action')
    -> [action]
    -> Kleisli Stochastic () action'
    -> Int 
    -> IO (QTable action' action)
learningStep _ q lens _ _ 0 = return q
learningStep game q lens possibleActions opponentStrategy n = do
  let agentStrat = strategyFromLens q lens
      gameOptic = play game (opponentStrategy ::- agentStrat ::- Nil)
      gameResultDist = decons $ runForward gameOptic ()
  (chosenAction, opponentAction, payoff) <- sample gameResultDist

  let q' = over lens (const (opponentAction, chosenAction, payoff, Nothing)) q

  learningStep game q' lens possibleActions opponentStrategy (n - 1)

runForward
  :: StochasticStatefulOptic s t a b
  -> s
  -> Stochastic a
runForward (StochasticStatefulOptic v _) s = do
  (_z, a) <- v s
  pure a

verifyStrategy :: 
    (Enum action, Bounded action, Ord action, Show action, Enum action', Bounded action', Ord action', Show action')
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action',
         Kleisli Stochastic action' action]
       '[[DiagnosticInfoBayesian () action'],
         [DiagnosticInfoBayesian action' action]]
       ()
       ()
       (action, action', Double)
       ()
    -> Kleisli Stochastic () action'
    -> IO ()
verifyStrategy game opponentStrategy = do
    learnedStrategy <- runOptimizerForExtensiveGame game opponentStrategy

    -- putStrLn $ runKleisli learnedStrategy ()
    pure ()

    -- cant do verification with game that outputs something, because evaluate context cant be void anymore then
    -- that should be no problem because we only want to learn a strategy
    -- the user needs to define the exact game twice, once with the payoff of the decision maker that should learn as the games output
    
    -- isEquilibriumGame game (learnedStrategy ::- opponentStrategy ::- Nil)

strategyFromLens :: 
    QTable action' action
    -> CustomLens (QTable action' action) (QTable action' action) (action' -> [(action, Double)]) (action', action, Double, Maybe action')
    -> Kleisli Stochastic action' action
strategyFromLens q lens = Kleisli $ \s ->
  distFromList $ view lens q s

isEquilibriumGame game strategyTuple = generateIsEq $ evaluate game strategyTuple void