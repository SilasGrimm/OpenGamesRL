{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Custom.SimpleGame where

import GHC.Generics

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLLens (RLLens(RLLens), nActions, State, Action, Reward, qLearningLens)
import OpenGames.Custom.ModifiedRLLens (stepEnvironment)


-- Try to get a basic decision game working, but getting compile errors for the 'singleDecisionVerbose game'

singleDecisionVerbose actionSpace payoffFunction = [opengame|
   inputs    :  currentState    ;
   feedback  :      ;

   :----------------------------:
   inputs    :  currentState  ;
   feedback  :      ;
   operation : dependentDecision "player1" (\y -> actionSpace)  ;
   outputs   : chosenAction ;
   returns   : payoffFunction currentState chosenAction  ;
   :----------------------------:

   outputs   :   chosenAction ;
   returns   :   payoffFunction currentState chosenAction  ;
  |]

-- | The same decision in the reduced style, i.e. ignoring empty fields
-- Requires a list of actions, and a payoff function
-- singleDecisionReduced actionSpace payoffFunction = [opengame|
--    operation : dependentDecision "player1" (const actionSpace);
--    outputs   : decisionPlayer1 ;
--    returns   : payoffFunction decisionPlayer1     ;
--   |]

------------------
-- 0.1. Parameters
-- Next, we provide additional modelling parameters

actionSpace = [1..nActions]

-- | Some arbitrary payoff function with a clear max
payoffFunction :: State -> Action -> Reward
payoffFunction = stepEnvironment

-- | Instantiate the single decision with parameters
-- We use the _payoff_ function with a peak at 5
gameSingleDecisionVerbose = singleDecisionVerbose actionSpace payoffFunction

----------------
-- 0.2. Analysis
-- We analyze the game and test whether certain actions are optimal
-- Here, this is trivial, but it is useful to see it first at that simple level


-- | This function defines a checker on the game _gameSingleDecisionVerbose_
-- It expects a strategy, here just a single action, as input and outputs information whether the action is optimal
-- This is done by the _evaluate_ function. In a game with several players the same _evaluate_ will check for an equilibrium condition
-- The _generateisEq_ transforms this output into an easier to digest format.
-- NOTE: _void_ represents a context relative to which a game is evaluated. As the game used has
-- no incoming or outcoming information, the context is empty (i.e. "void")
isOptimalSingleDecisionVerbose strat = generateIsEq $ evaluate gameSingleDecisionVerbose strat void


-- | Next, we define a strategu. We use the helper function _pureAction_ to determine a pure strategy
-- This is as simples as it gets, the strategy expects an action and will play this
-- action with certainty
pureIdentity :: Double -> List '[Kleisli Stochastic () Double]
pureIdentity action = pureAction action ::- Nil

-- | Now, we are ready to actually run this game and the optimality check. Example usages:
-- isOptimalSingleDecisionVerbose (pureIdentity 4), or
-- isOptimalSingleDecisionVerbose (pureIdentity 5),