{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.PrisonersDilemma where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.PrisonersDilemmaExternal (prisonersDilemmaMatrix, sample)
import OpenGames.Custom.RLLens (Action, Reward, QTable, State)
import OpenGames.Custom.ModifiedRLLens (qLearningLensNew)
import OpenGames.Custom.ModifiedRLLens
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import qualified Control.Applicative as Vector

------- Overall workflow / Minimal goal ------
-- 1. Learn strategy
-- 2. Use OpenGames to verify if strategy is in equilibrium

------- Small goal -------------
-- 3. Implement games for several problems
--    Implement with alternative RL algorithms

------- Large goal -------------
-- 4. Define Game, automatically learn strategy for game and check if in equilibrium
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

strategyFromLens :: QTable -> QLens QTable State Action Reward -> Kleisli Stochastic () Action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ deploy lens q 0

strategy1 :: Kleisli Stochastic () Action
strategy1 = strategyFromLens initialQTable qLearningLensNew

strategy2 :: Kleisli Stochastic () Action
strategy2 = strategyFromLens initialQTable qLearningLensNew

alwaysDefect = Kleisli $ \() ->
  distFromList $ [(0, 0), (1, 1)]

stratTuple = strategy1 ::- strategy1 ::- Nil
stratTuple2 = strategy1 ::- alwaysDefect ::- Nil

isEquilibriumPrisonersDilemmaCustom = generateIsEq $ evaluate prisonersDilemmaInternal stratTuple void

