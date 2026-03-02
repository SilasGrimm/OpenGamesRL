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

module OpenGames.Custom.Automated_QOptimizer_StrategicGame where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Custom.PrisonersDilemma_new_comp (Action(..), prisonersDilemmaMatrix)
import OpenGames.Custom.BoS_comp (ActionBoS(..))
import OpenGames.Custom.Gift_exchange_game_comp
import OpenGames.Custom.QLens_compositional
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)
import OpenGames.Engine.Diagnostics (PrintIsEq(..), Concat(..))

import System.Random (randomRIO)
import Numeric.Probability.Distribution hiding (map, lift)
import Control.Monad.State hiding (void, lift)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

-- createOptimizerLensForStrategicGameAgent :: 
--     (Enum action, Bounded action, Ord action)
--     => [action]
--     -> Double
--     -> Double
--     -> Double
--     -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
-- createOptimizerLensForStrategicGameAgent possibleActions alpha gamma epsilon = customQLens alpha gamma epsilon (const possibleActions)

-- runOptimizerForStrategicGame :: 
--     (Enum action, Bounded action, Ord action, Show action)
--     => ([action] -> [Double])
--     -> List s
--     -> IO (Kleisli Stochastic () action)
-- runOptimizerForStrategicGame payoffMatrix opponentStrategies = do
--     let
--         possibleActions :: (Enum action, Bounded action) => [action]
--         possibleActions = [minBound .. maxBound]

--         q = Map.fromList [((0, a), 0) | a <- possibleActions]

--     learnedQ <- learnStrategy q (createOptimizerLensForStrategicGameAgent possibleActions 0.1 0.95 0.1) possibleActions payoffMatrix opponentStrategies
--     putStrLn $ show (Map.toList learnedQ)

--     pure $ strategyFromLens learnedQ (createOptimizerLensForStrategicGameAgent possibleActions 0.1 0.95 0.0)

-- trainSteps = 200

-- learnStrategy :: 
--     QTable Int action 
--     -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
--     -> [action]
--     -> ([action] -> [Double])
--     -> List s
--     -> IO (QTable Int action)
-- learnStrategy q lens possibleActions payoffMatrix opponentStrategies = do
--   learningStep q lens possibleActions payoffMatrix opponentStrategies trainSteps -- train epsilon greedy for 150 steps/iterations of the game (since BoS is an one-shot game)
--   -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
--   -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- -- plays n games and returns learned strategy
-- learningStep :: 
--     QTable Int action 
--     -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
--     -> [action]
--     -> ([action] -> [Double])
--     -> List s
--     -> Int 
--     -> IO (QTable Int action)
-- learningStep q lens _ _ _ 0 = return q
-- learningStep q lens possibleActions payoffMatrix opponentStrategies n = do
--   let actionDist = view lens q 0
  
--   -- opponentActionIndex <- randomRIO (0 :: Int, length possibleActions - 1)
--   -- let opponentAction = possibleActions !! opponentActionIndex
--   let opponentActionDists = map (\k -> decons $ runKleisli k ()) opponentStrategies
--       opponentActionsIO = map sample opponentActionDists
--   opponentActions <- sequence opponentActionsIO
      
--   chosenAction <- sample actionDist -- sample from distribution

--   let payoff = head $ payoffMatrix (chosenAction : opponentActions)

-- --   putStrLn $ "Iteration: " ++ show (trainSteps - n)
-- --   putStrLn $ "QTable: " ++ show (toList q)
-- --   putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

--   let q' = over lens (const (0, chosenAction, payoff, Nothing)) q

--   learningStep q' lens possibleActions payoffMatrix opponentStrategies (n - 1)

-- verifyStrategy :: 
--     (Enum action, Bounded action, Ord action, Show action, 
--     MapL PrintIsEq d (ConstMap String d),
--     FoldrL Concat String (ConstMap String d)
--     )
--     => OpenGame
--        StochasticStatefulOptic
--        StochasticStatefulContext
--        s
--        d
--        ()
--        ()
--        ()
--        ()
--     -> ([action] -> [Double])
--     -> List '[Kleisli Stochastic () action, Kleisli Stochastic () action']
--     -> IO ()
-- verifyStrategy game payoffMatrix opponentStrategies  = do
--     learnedStrategy <- runOptimizerForStrategicGame payoffMatrix opponentStrategies

--     pure ()

    -- isEquilibriumGame game opponentStrategies

strategyFromLens :: 
    QTable Int action
    -> CustomLens (QTable Int action) (QTable Int action) (Int -> [(action, Double)]) (Int, action, Double, Maybe Int)
    -> Kleisli Stochastic () action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ view lens q 0

isEquilibriumGame game strategyTuple = generateIsEq $ evaluate game strategyTuple void

pdMatrix :: Action -> Action -> (Double, Double)
pdMatrix StaySilent StaySilent = (-1, -1)
pdMatrix Testify Testify = (-2, -2)
pdMatrix StaySilent Testify = (-3, 0)
pdMatrix Testify StaySilent = (0, -3)

bosMatrix :: ActionBoS -> ActionBoS -> (Double, Double)
bosMatrix Bach Bach = (2, 1)
bosMatrix Bach Stravinsky = (0, 0)
bosMatrix Stravinsky Bach = (0, 0)
bosMatrix Stravinsky Stravinsky = (1, 2)

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
   outputs   :  (prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2)  ;
   returns   :    ;
|]

prisonersDilemmaInternalWithAgentAction = [opengame|
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
   outputs   :  (decisionPlayer1, prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2)  ;
   returns   :    ;
|]

data DiffActionPlayer1 = Action11 | Action12 deriving (Eq, Ord, Show, Enum, Bounded)
data DiffActionPlayer2 = Action21 | Action22 deriving (Eq, Ord, Show, Enum, Bounded)

diffActionGamePayoffs :: DiffActionPlayer1 -> DiffActionPlayer2 -> (Double, Double)
diffActionGamePayoffs Action11 Action21 = (2, 1)
diffActionGamePayoffs Action11 Action22 = (2, 3)
diffActionGamePayoffs Action12 Action21 = (1, 2)
diffActionGamePayoffs Action12 Action22 = (0, 1)

checkDiffActionsGame = [opengame|
   inputs    :    ;
   feedback  :    ;
   :----------------------------:
   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player1" (const [Action11, Action12]) ;
   outputs   : decisionPlayer1 ;
   returns   : fst $ diffActionGamePayoffs decisionPlayer1 decisionPlayer2 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player2" (const [Action21, Action22]) ;
   outputs   : decisionPlayer2 ;
   returns   : snd $ diffActionGamePayoffs decisionPlayer1 decisionPlayer2 ;
   :----------------------------:
   outputs   :  (fst $ diffActionGamePayoffs decisionPlayer1 decisionPlayer2)  ;
   returns   :    ;
|]

diffActionGameStrat = Kleisli $ \() -> distFromList [(Action21, 1.0)]

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
    outputs   :  (decisionEmployee, decisionEmployer, gegPayoffMatrix Employee decisionEmployer decisionEmployee)  ;
    returns   :    ;
|]