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

module OpenGames.Custom.PrisonersDilemma_new_comp where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral (QTable, sample, createProbabilitiesFromRewards)
import OpenGames.Custom.QLens_compositional
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import System.Random (randomRIO)

type State = Int
data Action = Testify | StaySilent deriving (Eq, Ord, Show, Enum, Bounded)
type Reward = Double

prisonersDilemmaMatrix :: Action -> Action -> Reward
prisonersDilemmaMatrix StaySilent StaySilent = -1
prisonersDilemmaMatrix StaySilent Testify = -3
prisonersDilemmaMatrix Testify StaySilent = 0
prisonersDilemmaMatrix Testify Testify = -2

prisonersDilemmaInternal = [opengame|
   inputs    :    ;
   feedback  :    ;
   :----------------------------:
   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player1" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player2" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   :    ;
   returns   :    ;
|]

initialQTable :: QTable State Action
initialQTable = Map.fromList [((0, StaySilent), 0), ((0, Testify), 0)]

pdLens :: CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State)
pdLens = customQLens 0.2 0.95 0.1 (const [Testify, StaySilent])
pdLensGreedy :: CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State)
pdLensGreedy = customQLens 0.2 0.95 0.0 (const [Testify, StaySilent])

-- training
learnPDStrategyComp :: QTable State Action -> CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State) -> IO (QTable State Action)
learnPDStrategyComp q lens = do
  learningStepComp q lens 150
  

-- plays n games and returns learned strategy
-- we now have the following advantages: 
  --  When the underlying Q-Table structure changes (e.g. to a list), we do not have du change how we get a qValue when using the structure, since the lens hides this implementation for us
  --    and we only have to use the view function to get the function that implementes this logic
learningStepComp :: QTable State Action -> CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State) -> Int -> IO (QTable State Action)
learningStepComp q lens 0 = return q
learningStepComp q lens n = do
--   let qFocus = view lens q -- forward pass returns a function that allows looking into the qTable
--       actionDist = createProbabilitiesFromRewards [ (a, qFocus (0, a))
--                 | a <- [Testify, StaySilent] ] 0.1
  let actionDist = (view lens q) 0 -- forward pass returns a function that takes the current state and outputs the policy, which maps states to proabability distributions

      opponentAction = Testify
  
  chosenAction <- sample actionDist -- sample from distribution

  let payoff = prisonersDilemmaMatrix chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (150 - n)
  putStrLn $ "QTable: " ++ show (Map.toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let  q' = over lens (const (0, chosenAction, payoff, Nothing)) q

  learningStepComp q' lens (n - 1)

verifyStrategy :: IO (QTable State Action) -> CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State) -> IO ()
verifyStrategy ioQ lens = do
    q <- ioQ
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumPrisonersDilemmaCustom (learnedStrategy ::- alwaysTestify ::- Nil)

checkPDAgent = verifyStrategy (learnPDStrategyComp initialQTable pdLens) pdLens

strategyFromLens :: QTable State Action -> CustomLens (QTable State Action) (QTable State Action) (State -> [(Action, Double)]) (State, Action, Double, Maybe State) -> Kleisli Stochastic () Action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ view lens q 0

strategy1 :: Kleisli Stochastic () Action
strategy1 = strategyFromLens initialQTable pdLens

strategy2 :: Kleisli Stochastic () Action
strategy2 = strategyFromLens initialQTable pdLens

alwaysTestify = Kleisli $ \() ->
  distFromList [(StaySilent, 0), (Testify, 1)]

alwaysStaySilent = Kleisli $ \() ->
  distFromList [(StaySilent, 1), (Testify, 0)]

stratTuple = strategy1 ::- strategy1 ::- Nil
stratTuple2 = strategy1 ::- alwaysTestify ::- Nil

bothTestifyStrat = alwaysTestify ::- alwaysTestify ::- Nil

isEquilibriumPrisonersDilemmaCustom strategyTuple = generateIsEq $ evaluate prisonersDilemmaInternal strategyTuple void