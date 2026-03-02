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

module OpenGames.Custom.Automated_QOptimizer_Bimatrix where

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

-- This implementation learns a Q-Agent for an arbitrary bimatrix game
-- Always the first defined dependentDecision in the game is learned
--  This does not impose any constraints on the user except defining the decision maker that they want to learn at first
--  For simultaneous games, this doesnt change the dynamics of the game
-- To generalize this to strategic games, it must be made possible to learn games with more than two players
--  This is difficult, since the heterogenous type lists needed for the player strategies need to be given at compile time (i.e. the exact length needs to be known)
-- However, if all players have the same actions, we could be able to generalize with Replicate, allowing the user to define games with more players at compile time

createOptimizerLensForBimatrixGameAgent :: 
    (Enum action, Bounded action, Ord action)
    => [action]
    -> Double
    -> Double
    -> Double
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
createOptimizerLensForBimatrixGameAgent possibleActions alpha gamma epsilon = customQLens alpha gamma epsilon (const possibleActions)

runOptimizerForBimatrixGame :: 
    (Enum action, Bounded action, Ord action, Show action)
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       (action, Double)
       ()
    -> Kleisli Stochastic () action'
    -> IO (Kleisli Stochastic () action)
runOptimizerForBimatrixGame game opponentStrategy = do
    let
        possibleActions :: (Enum action, Bounded action) => [action]
        possibleActions = [minBound .. maxBound]

        q = Map.fromList [((0, a), 0) | a <- possibleActions]

    learnedQ <- learnStrategy game q (createOptimizerLensForBimatrixGameAgent possibleActions 0.1 0.95 0.1) possibleActions opponentStrategy
    putStrLn $ show (Map.toList learnedQ)

    pure $ strategyFromLens learnedQ (createOptimizerLensForBimatrixGameAgent possibleActions 0.1 0.95 0.0)

trainSteps = 200

learnStrategy :: 
  (Show action) =>
    OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       (action, Double)
       ()
    -> QTable Int action 
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> [action]
    -> Kleisli Stochastic () action'
    -> IO (QTable Int action)
learnStrategy game q lens possibleActions opponentStrategy = do
  learningStep game q lens possibleActions opponentStrategy trainSteps

-- plays n games and returns learned strategy
learningStep :: 
    (Show action) =>
    OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       (action, Double) -- agent reward
       ()
    -> QTable Int action 
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> [action]
    -> Kleisli Stochastic () action'
    -> Int 
    -> IO (QTable Int action)
learningStep _ q lens _ _ 0 = return q
learningStep game q lens possibleActions opponentStrategy n = do

  let agentStrat = strategyFromLens q lens
      gameOptic = play game (agentStrat ::- opponentStrategy ::- Nil)
      gameResultDist = decons $ runForward gameOptic () -- relies on the game to output probability distributions over the payoffs and actions for the player that should be learned
  (chosenAction, payoff) <- sample gameResultDist

  let q' = over lens (const (0, chosenAction, payoff, Nothing)) q

  learningStep game q' lens possibleActions opponentStrategy (n - 1)

runForward
  :: StochasticStatefulOptic s t a b
  -> s
  -> Stochastic a
runForward (StochasticStatefulOptic v _) s = do
  (_z, a) <- v s
  pure a

verifyStrategy :: 
    (Enum action, Bounded action, Ord action, Show action)
    => OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       (action, Double)
       ()
    -> Kleisli Stochastic () action'
    -> IO ()
verifyStrategy game opponentStrategy = do
    learnedStrategy <- runOptimizerForBimatrixGame game opponentStrategy

    -- putStrLn $ runKleisli learnedStrategy ()
    pure ()

    -- cant do verification with game that outputs something, because evaluate context cant be void anymore then
    -- that should be no problem because we only want to learn a strategy
    -- the user needs to define the exact game twice, once with the payoff of the decision maker that should learn as the games output
    
    -- isEquilibriumGame game (learnedStrategy ::- opponentStrategy ::- Nil)

strategyFromLens :: 
    QTable Int action
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> Kleisli Stochastic () action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ view lens q 0

isEquilibriumGame game strategyTuple = generateIsEq $ evaluate game strategyTuple void