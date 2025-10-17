{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.PD_repeated where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.PrisonersDilemmaExternal (prisonersDilemmaMatrix, sample, playStep, playQLearning)
import OpenGames.Custom.RLLens (Action, Reward, QTable, State)
import OpenGames.Custom.ModifiedRLLens (qLearningLensNew)
import OpenGames.Custom.ModifiedRLLens
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector

------- Overall workflow / Minimal goal ------
-- 1. Learn strategy
-- 2. Use OpenGames to verify if strategy is in equilibrium

------- Small goal -------------
-- 3. Implement games for several problems
--    Implement with alternative RL algorithms

------- Large goal -------------
-- 4. Let user define game, automatically learn strategy for game and check if in equilibrium
--    Find class of games where this is possible

-- Einführung:
--    RL, Category theory, OpenGames

prisonersDilemmaInternal = [opengame|
   inputs    :    ;
   feedback  :    ;
   :----------------------------:
   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player1" (const [0, 1]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player2" (const [0, 1]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   :    ;
   returns   :    ;
|]

initialQTable = Map.fromList [((0, 0), 0), ((0, 1), 0)]

learnPDStrategy :: QTable -> QLens QTable State Action Reward -> IO QTable
learnPDStrategy q lens = do
  learningStep q lens 150
  

-- plays n games and returns learned strategy
learningStep :: QTable -> QLens QTable State Action Reward -> Int -> IO QTable
learningStep q lens 0 = return q
learningStep q lens n = do
  let actionDist = deploy lens q 0
      opponentAction = 1
  
  chosenAction <- sample actionDist -- sample from distribution

  let payoff = prisonersDilemmaMatrix chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (150 - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = adapt lens q (0, chosenAction, payoff, 0)

  learningStep q' lens (n - 1)

verifyStrategy :: IO QTable -> QLens QTable State Action Reward -> IO ()
verifyStrategy ioQ lens = do
    q <- ioQ
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumPrisonersDilemmaCustom (learnedStrategy ::- alwaysDefect ::- Nil)

checkPDAgent = verifyStrategy (learnPDStrategy initialQTable qLearningLensNew) qLearningLensNewGreedy

strategyFromLens :: QTable -> QLens QTable State Action Reward -> Kleisli Stochastic () Action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ deploy lens q 0

strategy1 :: Kleisli Stochastic () Action
strategy1 = strategyFromLens initialQTable qLearningLensNew

strategy2 :: Kleisli Stochastic () Action
strategy2 = strategyFromLens initialQTable qLearningLensNew

alwaysDefect = Kleisli $ \() ->
  distFromList $ [(0, 0), (1, 1)]

alwaysStaySilent = Kleisli $ \() ->
  distFromList $ [(0, 1), (1, 0)]

stratTuple = strategy1 ::- strategy1 ::- Nil
stratTuple2 = strategy1 ::- alwaysDefect ::- Nil

isEquilibriumPrisonersDilemmaCustom strategyTuple = generateIsEq $ evaluate prisonersDilemmaInternal strategyTuple void

