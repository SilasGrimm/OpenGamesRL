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
       Double
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
    OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       Double
       ()
    -> QTable Int action 
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> [action]
    -> Kleisli Stochastic () action'
    -> IO (QTable Int action)
learnStrategy game q lens possibleActions opponentStrategy = do
  learningStep game q lens possibleActions opponentStrategy trainSteps -- train epsilon greedy for 150 steps/iterations of the game (since BoS is an one-shot game)
  -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
  -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- plays n games and returns learned strategy
learningStep :: 
    OpenGame
       StochasticStatefulOptic
       StochasticStatefulContext
       '[Kleisli Stochastic () action,
         Kleisli Stochastic () action']
       '[[DiagnosticInfoBayesian () action],
         [DiagnosticInfoBayesian () action']]
       ()
       ()
       Double -- agent reward
       ()
    -> QTable Int action 
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> [action]
    -> Kleisli Stochastic () action'
    -> Int 
    -> IO (QTable Int action)
learningStep _ q lens _ _ 0 = return q
learningStep game q lens possibleActions opponentStrategy n = do
  let actionDist = view lens q 0
  
  -- let opponentActionDist = decons $ runKleisli opponentStrategy ()
  -- opponentAction <- sample opponentActionDist

  chosenAction <- sample actionDist -- sample from distribution

  let agentStrat = Kleisli $ \_ -> distFromList [(chosenAction, 1.0)] -- choose samplede strat with probability 1
      gameOptic = play game (agentStrat ::- opponentStrategy ::- Nil)
      gamePayoff = decons $ runForward gameOptic () -- relies on the game to output the payoffs for the player that should be learned
  payoff <- sample gamePayoff

      
  -- let payoff = fst $ payoffMatrix chosenAction opponentAction


--   putStrLn $ "Iteration: " ++ show (trainSteps - n)
--   putStrLn $ "QTable: " ++ show (toList q)
--   putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

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
       Double
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

-- pdMatrix :: Action -> Action -> (Double, Double)
-- pdMatrix StaySilent StaySilent = (-1, -1)
-- pdMatrix Testify Testify = (-2, -2)
-- pdMatrix StaySilent Testify = (-3, 0)
-- pdMatrix Testify StaySilent = (0, -3)

-- bosMatrix :: ActionBoS -> ActionBoS -> (Double, Double)
-- bosMatrix Bach Bach = (2, 1)
-- bosMatrix Bach Stravinsky = (0, 0)
-- bosMatrix Stravinsky Bach = (0, 0)
-- bosMatrix Stravinsky Stravinsky = (1, 2)

-- pdAlwaysTestify :: Kleisli IO () [(Action, Double)]
-- pdAlwaysTestify = Kleisli $  \() -> pure [(Testify, 1.0)]

-- bosAlwaysStravinsky :: Kleisli IO () [(ActionBoS, Double)]
-- bosAlwaysStravinsky = Kleisli $ \() -> pure [(Stravinsky, 1.0)]