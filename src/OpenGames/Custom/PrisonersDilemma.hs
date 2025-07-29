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
import Control.Arrow (Kleisli)
import qualified Control.Applicative as Vector

prisonersDilemmaInternal qTable1 qTable2 = [opengame|
   inputs    :    ;
   feedback  : (payoff1,payoff2)     ;
   :----------------------------:
   inputs    :  qTable1    ;
   feedback  : payoff1    ;
   operation : dependentDecision "player1" (const [0, 1]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    : qTable2     ;
   feedback  : payoff2    ;
   operation : dependentDecision "player2" (const [0, 1]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
|]

initialQTable = Map.fromList [((0, 0), 0), ((0, 1), 0)]

-- 0 for staying silent, 1 for confessing
strategyFromLens :: QLens QTable State Action Reward -> Kleisli Stochastic QTable Action
strategyFromLens lens = Kleisli $ \qTable ->
  distFromList $ deploy lens qTable 0

strategy1 :: Kleisli Stochastic QTable Action
strategy1 = strategyFromLens qLearningLensNew

strategy2 :: Kleisli Stochastic QTable Action
strategy2 = strategyFromLens qLearningLensNew

stratTuple = strategy1 ::- strategy1 ::- Nil

env :: StochasticStatefulContext () ((), ()) (Action, Action) ()
env = StochasticStatefulContext
    (return ((initialQTable, initialQTable), ()))
    (\(q1, q2) (a1, a2) -> do 
        let r1 = prisonersDilemmaMatrix a1 a2
            r2 = prisonersDilemmaMatrix a2 a1
            q1' = adapt qLearningLensNew q1 (0, a1, r1, 0) -- we are always in state 0 for prisoners dilemma because there is only one state
            q2' = adapt qLearningLensNew q2 (0, a2, r2, 0)
        return ()
    )

isEquilibriumPrisonersDilemmaRepeatedCustom = generateIsEq $ evaluate (prisonersDilemmaInternal initialQTable initialQTable) stratTuple env

