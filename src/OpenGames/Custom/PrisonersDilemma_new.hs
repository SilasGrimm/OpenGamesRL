{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.PrisonersDilemma_new where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral (qLearningLens', qLearningGreedyLens', QTable, QLens', deploy', adapt', sample)
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import System.Random (randomRIO)

type State = Int
data Action = Testify | StaySilent deriving (Eq, Ord, Show)
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

initialQTable = Map.fromList [((0, StaySilent), 0), ((0, Testify), 0)]

pdLens = qLearningLens' 0.2 0.2 0.95 (const [Testify, StaySilent])
pdLensGreedy = qLearningGreedyLens' 0.2 0.95 (const [Testify, StaySilent])

learnPDStrategy :: QTable State Action -> QLens' (QTable State Action) State Action Reward -> IO (QTable State Action)
learnPDStrategy q lens = do
  learningStep q lens 150
  

-- plays n games and returns learned strategy
learningStep :: QTable State Action -> QLens' (QTable State Action) State Action Reward -> Int -> IO (QTable State Action)
learningStep q lens 0 = return q
learningStep q lens n = do
  let actionDist = deploy' lens q 0
      opponentAction = Testify
  
  chosenAction <- sample actionDist -- sample from distribution

  let payoff = prisonersDilemmaMatrix chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (150 - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = adapt' lens q (0, chosenAction, payoff, Nothing)

  learningStep q' lens (n - 1)

verifyStrategy :: IO (QTable State Action) -> QLens' (QTable State Action) State Action Reward -> IO ()
verifyStrategy ioQ lens = do
    q <- ioQ
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumPrisonersDilemmaCustom (learnedStrategy ::- alwaysTestify ::- Nil)

checkPDAgent = verifyStrategy (learnPDStrategy initialQTable pdLens) pdLensGreedy

strategyFromLens :: QTable State Action -> QLens' (QTable State Action) State Action Reward -> Kleisli Stochastic () Action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ deploy' lens q 0

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

