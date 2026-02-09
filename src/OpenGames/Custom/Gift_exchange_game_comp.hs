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

module OpenGames.Custom.Gift_exchange_game_comp where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Custom.QLens_compositional
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

data Player = Employer | Employee deriving (Eq, Ord, Show)
data EmployerAction = HighSalary | LowSalary deriving (Eq, Ord, Show, Enum, Bounded)
data EmployeeAction = HighEffort | LowEffort deriving (Eq, Ord, Show, Enum, Bounded)

gegPayoffMatrix :: Player -> EmployerAction -> EmployeeAction -> Reward
gegPayoffMatrix Employer HighSalary HighEffort = 2
gegPayoffMatrix Employer HighSalary LowEffort = 0
gegPayoffMatrix Employer LowSalary HighEffort = 3
gegPayoffMatrix Employer LowSalary LowEffort = 1

gegPayoffMatrix Employee HighSalary HighEffort = 2
gegPayoffMatrix Employee HighSalary LowEffort = 3
gegPayoffMatrix Employee LowSalary HighEffort = 0
gegPayoffMatrix Employee LowSalary LowEffort = 1

giftExchangeGame = [opengame|
    inputs    :    ;
    feedback  :    ;
    :----------------------------:
    inputs    :    ;
    feedback  :    ;
    operation : dependentDecision "employer" (const [HighSalary, LowSalary]) ;
    outputs   : decisionEmployer ;
    returns   : gegPayoffMatrix Employer decisionEmployer decisionEmployee ;

    inputs    :  decisionEmployer  ;
    feedback  :     ;
    operation : dependentDecision "employee" (const [HighEffort, LowEffort]) ;
    outputs   : decisionEmployee ;
    returns   : gegPayoffMatrix Employee decisionEmployer decisionEmployee ;
    :----------------------------:
    outputs   :    ;
    returns   :    ;
|]

initialQTableEmployer = Map.fromList [((0, LowSalary), 0), ((0, HighSalary), 0)]
initialQTableEmployee = Map.fromList [((LowSalary, LowEffort), 0), ((LowSalary, HighEffort), 0), ((HighSalary, LowEffort), 0), ((HighSalary, HighEffort), 0)]

gegLensEmployer:: CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int)
gegLensEmployer = customQLens 0.1 0.8 0.1 (const [HighSalary, LowSalary])
--gegGreedyLensEmployer = qLearningGreedyLens 0.5 0.95 (const [HighSalary, LowSalary]) 
-- employer Learning does not work because greedy lens sets Prob(HighSalary) = 0 and we condition on this -> division by 0
-- therefore only epsilon greedy with very small epsilon and use epsilon-Nash Equilibrium

gegGreedyLensEmployer:: CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int)
gegGreedyLensEmployer = customQLens 0.1 0.95 0.0 (const [HighSalary, LowSalary])

gegLensEmployee :: CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction)
gegLensEmployee = customQLens 0.1 0.95 0.1 (const [HighEffort, LowEffort])

gegGreedyLensEmployee :: CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction)
gegGreedyLensEmployee = customQLens 0.1 0.95 0.0 (const [HighEffort, LowEffort])

trainSteps = 350 -- roughly how long it takes to have Prob(LowEffort | HighSalary) > Prob(HighEffort | HighSalary)

learnEmployerGEGStrategy :: 
    QTable Int EmployerAction 
    -> CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int) 
    -> IO (QTable Int EmployerAction)
learnEmployerGEGStrategy q lens = do
  employerLearningStep q lens (trainSteps + 100) -- extra steps for employer training

-- plays n games and returns learned strategy
employerLearningStep :: QTable Int EmployerAction -> CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int) -> Int -> IO (QTable Int EmployerAction)
employerLearningStep q lens 0 = return q
employerLearningStep q lens n = do
  let actionDist = view lens q 0
      opponentAction = LowEffort

  chosenAction <- sample actionDist -- sample from distribution

  let payoff = gegPayoffMatrix Employer chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (trainSteps - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = over lens (const (0, chosenAction, payoff, Nothing)) q

  employerLearningStep q' lens (n - 1)


learnEmployeeGEGStrategy :: 
    QTable EmployerAction EmployeeAction 
    -> CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction)
    -> IO (QTable EmployerAction EmployeeAction)
learnEmployeeGEGStrategy q lens = do
  employeeLearningStep q lens trainSteps

employeeLearningStep :: 
    QTable EmployerAction EmployeeAction 
    -> CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction)
    -> Int 
    -> IO (QTable EmployerAction EmployeeAction)
employeeLearningStep q lens 0 = return q
employeeLearningStep q lens n = do
  let opponentAction = LowSalary
      actionDist = view lens q opponentAction

  chosenAction <- sample actionDist -- sample from distribution

  let payoff = gegPayoffMatrix Employee opponentAction chosenAction

  putStrLn $ "Iteration: " ++ show (trainSteps - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = over lens (const (opponentAction, chosenAction, payoff, Nothing)) q
      -- q'' = adapt lens q' (HighSalary, chosenAction, gegPayoffMatrix Employee HighSalary chosenAction, HighSalary)

  employeeLearningStep  q' lens (n - 1)

verifyEmployeeStrategy :: IO (QTable EmployerAction EmployeeAction) -> CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction) -> Kleisli Stochastic () EmployerAction -> IO ()
verifyEmployeeStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    let learnedStrategy = employeeStrategyFromLens q lens

    isEquilibriumGEGCustom (opponentStrategy ::- learnedStrategy ::- Nil)

verifyEmployerStrategy :: IO (QTable Int EmployerAction) -> CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int) -> Kleisli Stochastic EmployerAction EmployeeAction -> IO ()
verifyEmployerStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    let learnedStrategy = employerStrategyFromLens q lens

    isEquilibriumGEGCustom (learnedStrategy ::- opponentStrategy ::- Nil)

-- learn a epsilon greedy strategy and then use the learned qTable greedily to reach the nash equilibrium
checkEmployerGEGAgent opponentStrategy = verifyEmployerStrategy (learnEmployerGEGStrategy initialQTableEmployer gegLensEmployer) gegGreedyLensEmployer opponentStrategy
checkEmployeeGEGAgent opponentStrategy = verifyEmployeeStrategy (learnEmployeeGEGStrategy initialQTableEmployee gegLensEmployee) gegGreedyLensEmployee opponentStrategy

-- changed this to a fully pure strategy by extracting the maximum action which should be executed with a probability of 1
-- and only putting this action into the distFromList function
--    -> This is equivalent to what we have done in the opponentStrategies below
employerStrategyFromLens :: QTable Int EmployerAction -> CustomLens (QTable Int EmployerAction) (QTable Int EmployerAction) (Int -> [(EmployerAction, Double)]) (Int, EmployerAction, Double, Maybe Int) -> Kleisli Stochastic () EmployerAction
employerStrategyFromLens q lens = Kleisli $ \() ->
  let actionDist = view lens q 0
      maxReward = maximum $ map snd actionDist
      maxActionRewardPairDist = filter (\(a, r) -> r == maxReward) actionDist
  in distFromList maxActionRewardPairDist

employeeStrategyFromLens :: QTable EmployerAction EmployeeAction -> CustomLens (QTable EmployerAction EmployeeAction) (QTable EmployerAction EmployeeAction) (EmployerAction -> [(EmployeeAction, Double)]) (EmployerAction, EmployeeAction, Double, Maybe EmployerAction) -> Kleisli Stochastic EmployerAction EmployeeAction
employeeStrategyFromLens q lens = Kleisli $ \employerAction ->
  distFromList $ view lens q employerAction

-- leads to equilibrium
-- cant include (HighSalary, 0) in this because for sequential games all paths are evaluated and conditional probabilities computed -> division by 0 results in NaN
--   -> However, behaviour is not changed by this (is equivalent to a pureAction on LowSalary), so this should be okay
employerAlwaysLowSalary = Kleisli $ \() ->
  distFromList $ [(LowSalary, 1)]

-- does not lead to equilibrium
employerAlwaysHighSalary = Kleisli $ \() ->
  distFromList $ [(HighSalary, 1)]

employeeAlwaysLowEffort = Kleisli $ \employerAction ->
  distFromList $ [(LowEffort, 1)]
  
employeeAlwaysHighEffort = Kleisli $ \employerAction ->
  distFromList $ [(HighEffort, 1)]

isEquilibriumGEGCustom strategyTuple = generateIsEq $ evaluate giftExchangeGame strategyTuple void