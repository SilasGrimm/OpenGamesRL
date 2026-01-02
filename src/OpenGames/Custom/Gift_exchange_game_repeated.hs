{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.Gift_exchange_game_repeated where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

data Player = Employer | Employee deriving (Eq, Ord, Show)
data EmployerAction = HighSalary | LowSalary deriving (Eq, Ord, Show)
data EmployeeAction = HighEffort | LowEffort deriving (Eq, Ord, Show)

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

gegLensEmployer = qLearningLens 0.1 0.1 0.5 (const [HighSalary, LowSalary])
--gegGreedyLensEmployer = qLearningGreedyLens 0.5 0.95 (const [HighSalary, LowSalary]) 
-- employer Learning does not work because greedy lens sets Prob(HighSalary) = 0 and we condition on this -> division by 0
-- therefore only epsilon greedy with very small epsilon and use epsilon-Nash Equilibrium
gegGreedyLensEmployer = qLearningLens 0.00001 0.00001 0.95 (const [HighSalary, LowSalary])


gegLensEmployee = qLearningLens 0.2 0.2 0.5 (const [HighEffort, LowEffort])
gegGreedyLensEmployee = qLearningGreedyLens 0.5 0.95 (const [HighEffort, LowEffort])

trainSteps = 350 -- roughly how long it takes to have Prob(LowEffort | HighSalary) > Prob(HighEffort | HighSalary)

learnEmployerGEGStrategy :: QTable Int EmployerAction -> QLens (QTable Int EmployerAction) Int EmployerAction Reward -> IO (QTable Int EmployerAction)
learnEmployerGEGStrategy q lens = do
  employerLearningStep q lens (trainSteps + 100) -- extra steps for employer training

-- plays n games and returns learned strategy
employerLearningStep :: QTable Int EmployerAction -> QLens (QTable Int EmployerAction) Int EmployerAction Reward -> Int -> IO (QTable Int EmployerAction)
employerLearningStep q lens 0 = return q
employerLearningStep q lens n = do
  let actionDist = deploy lens q 0
      opponentAction = if even n then LowEffort else HighEffort

  chosenAction <- sample actionDist -- sample from distribution

  let payoff = gegPayoffMatrix Employer chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (trainSteps - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = adapt lens q (0, chosenAction, payoff, 0)

  employerLearningStep q' lens (n - 1)


learnEmployeeGEGStrategy :: QTable EmployerAction EmployeeAction -> QLens (QTable EmployerAction EmployeeAction) EmployerAction EmployeeAction Reward -> IO (QTable EmployerAction EmployeeAction)
learnEmployeeGEGStrategy q lens = do
  employeeLearningStep q lens trainSteps

employeeLearningStep :: QTable EmployerAction EmployeeAction -> QLens (QTable EmployerAction EmployeeAction) EmployerAction EmployeeAction Reward -> Int -> IO (QTable EmployerAction EmployeeAction)
employeeLearningStep q lens 0 = return q
employeeLearningStep q lens n = do
  let opponentAction = if even n then LowSalary else HighSalary -- employee needs to be trained with both actions of employer to ensure that equilibrium is reached on all paths
      actionDist = deploy lens q opponentAction

  chosenAction <- sample actionDist -- sample from distribution

  let payoff = gegPayoffMatrix Employee opponentAction chosenAction

  putStrLn $ "Iteration: " ++ show (trainSteps - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = adapt lens q (opponentAction, chosenAction, payoff, opponentAction)
      -- q'' = adapt lens q' (HighSalary, chosenAction, gegPayoffMatrix Employee HighSalary chosenAction, HighSalary)

  employeeLearningStep  q' lens (n - 1)

verifyEmployeeStrategy :: IO (QTable EmployerAction EmployeeAction) -> QLens (QTable EmployerAction EmployeeAction) EmployerAction EmployeeAction Reward -> Kleisli Stochastic () EmployerAction -> IO ()
verifyEmployeeStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    let learnedStrategy = employeeStrategyFromLens q lens

    isEquilibriumGEGCustom (opponentStrategy ::- learnedStrategy ::- Nil)

verifyEmployerStrategy :: IO (QTable Int EmployerAction) -> QLens (QTable Int EmployerAction) Int EmployerAction Reward -> Kleisli Stochastic EmployerAction EmployeeAction -> IO ()
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
employerStrategyFromLens :: QTable Int EmployerAction -> QLens (QTable Int EmployerAction) Int EmployerAction Reward -> Kleisli Stochastic () EmployerAction
employerStrategyFromLens q lens = Kleisli $ \() ->
  let actionDist = deploy lens q 0
      maxReward = maximum $ map snd actionDist
      maxActionRewardPairDist = filter (\(a, r) -> r == maxReward) actionDist
  in distFromList maxActionRewardPairDist

employeeStrategyFromLens :: QTable EmployerAction EmployeeAction -> QLens (QTable EmployerAction EmployeeAction) EmployerAction EmployeeAction Reward -> Kleisli Stochastic EmployerAction EmployeeAction
employeeStrategyFromLens q lens = Kleisli $ \employerAction ->
  distFromList $ deploy lens q employerAction

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


-- Beispiele für Definitionen etc. nutzen und erklären
-- Alle genutzten nicht-trivialen Wörter definieren und verstehen
-- Einheitliche Begriffe

-- Deep Q-Learning als optionales Ziel

-- Minimal goals: 1. Ohne 'Agent' und 'categorical' schreiben
--                differentiate probabilistic and deterministic games
--                  simultaneous, sequential, determinstic, probabilistic
--                4 games -> One-Shot/Simultaneous deterministic, One-Shot/Simultaneous probabilistic,
--                           Sequential deterministic, Sequential probabilistic
--                Multi-agent as optional goal
-- Remove research questions
-- given open games (implemented as lenses), can we compile an open game to the q learning structure
-- optional goal: one more practical application (not just a game) -> e.g. revenue management
-- Grundbausteine einer Martksituation identifizieren, Spiel auf Basis dieser Bausteine definieren um Strategie für gegebene Marktsituation zu testen
--        -> Composing games into complex institutions

-- Einführungsvortrag: Beispiele
--                     1. Motivation -> Game Theory, Open Games etc.
--                     2. Lenses (mit Beispiel)
--                     3. Spiel compositional (noch nicht automatisiert)
--                     4. Q-Learning lenses
--                     5. Ziele (Optionalziele, dann Minimalziele)