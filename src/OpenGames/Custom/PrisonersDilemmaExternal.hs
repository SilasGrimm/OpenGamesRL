{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}


module OpenGames.Custom.PrisonersDilemmaExternal where

import GHC.Generics (Generic)
import OpenGames.Preprocessor
import OpenGames.Custom.RLLens (Action, Reward, State, QTable)
import OpenGames.Custom.ModifiedRLLens
import OpenGames.Engine.Engine hiding (fromLens, fromFunctions, state)
import OpenGames.Custom.ModifiedRLLens (qLearningLensNew)
import OpenGames.Engine.ExternalEnvironment (interactWithEnv, ExternalEnvironmentGame, fromFunctions, fromLens, extractPayoffAndNextState)
import OpenGames.Engine.BayesianGames (dependentDecision)
import Numeric.Probability.Distribution hiding (map, lift, filter)

import Data.Map (Map)
import qualified Data.Map as Map

import System.Random (randomRIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (maximum)

-- 0 for staying silent, 1 for testifying
prisonersDilemmaMatrix :: Action -> Action -> Reward
prisonersDilemmaMatrix 0 0 = 3
prisonersDilemmaMatrix 0 1 = 0
prisonersDilemmaMatrix 1 0 = 5
prisonersDilemmaMatrix 1 1 = 1

-- Draw one sample from a probability distribution
sample :: [(Action, Prob)] -> IO Action
sample dist = do
    let cumulative = scanl1 (\(_, acc) (a, p) -> (a, acc + p)) dist
    r <- randomRIO (0, 1)  -- generate random Double in [0,1]
    return $ fst $ head $ dropWhile (\(_, p) -> p < r) cumulative

playQLearning :: IO ()
playQLearning = 
        liftIO $ 
            do 
            let player1Lens = qLearningLensNew
                player1QTable = Map.fromList [((0, 0), 0), ((0, 1), 0)] -- we initialize with reward 0 for both actions

                
                player2Lens = qLearningLensNew
                player2QTable = Map.fromList [((0, 0), 0), ((0, 1), 0)] -- we initialize with reward 0 for both actions
                

            playStep player1QTable player2QTable 3

playStep :: QTable -> QTable -> Int -> IO ()
playStep _ _ 0 = return ()
playStep player1QTable player2QTable n = do
                let
                    player1CurrentState = 0 -- note that there is only one state for the prisoners dilemma
                    player2CurrentState = 0 -- note that there is only one state for the prisoners dilemma
                    player1ActionDist = deploy qLearningLensNew player1QTable player1CurrentState
                    player2ActionDist = deploy qLearningLensNew player2QTable player2CurrentState
                -- choose action based on distribution --
                
                actionPlayer1 <- sample player1ActionDist
                actionPlayer2 <- sample player2ActionDist

                let strategy = actionPlayer1 ::- actionPlayer2 ::- Nil
                    nextgame = play prisonersDilemmaExternal strategy
                ((payoffPlayer1, payoffPlayer2), _) <- extractPayoffAndNextState nextgame () ()

                putStrLn $ "Action1: " ++ show actionPlayer1 ++ " | Reward1: " ++ show payoffPlayer1 ++ " | QTable1: " ++ show player1ActionDist
                putStrLn $ "Action2: " ++ show actionPlayer2 ++ " | Reward2: " ++ show payoffPlayer2 ++ " | QTable2: " ++ show player2ActionDist

                let player1q' = adapt qLearningLensNew player1QTable (0, actionPlayer1, payoffPlayer1, 0)
                    player2q' = adapt qLearningLensNew player2QTable (0, actionPlayer2, payoffPlayer2, 0)


                print player1q'
                print $ distFromList $ Map.toList player1q'

                playStep player1q' player2q' (n - 1)
                

prisonersDilemmaExternal :: ExternalEnvironmentGame
                              '[Action, Action]
                              '[]
                              ()
                              (Double, Double)
                              (Action, Action)
                              ()
prisonersDilemmaExternal = [opengame|
   inputs    :      ;
   feedback  : (payoff1,payoff2)     ;
   :----------------------------:
   inputs    :      ;
   feedback  : payoff1    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  : payoff2    ;
   operation : interactWithEnv ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
  |]


-- singleDecisionVerbose actionSpace payoffFunction = [opengame|
--    inputs    :  currentState    ;
--    feedback  :      ;

--    :----------------------------:
--    inputs    :  currentState  ;
--    feedback  :      ;
--    operation : dependentDecision "player1" (\y -> actionSpace)  ;
--    outputs   : chosenAction ;
--    returns   : payoffFunction currentState chosenAction  ;
--    :----------------------------:

--    outputs   :   chosenAction ;
--    returns   :   payoffFunction currentState chosenAction  ;
--   |]

-- -- | The same decision in the reduced style, i.e. ignoring empty fields
-- -- Requires a list of actions, and a payoff function
-- -- singleDecisionReduced actionSpace payoffFunction = [opengame|
-- --    operation : dependentDecision "player1" (const actionSpace);
-- --    outputs   : decisionPlayer1 ;
-- --    returns   : payoffFunction decisionPlayer1     ;
-- --   |]

-- ------------------
-- -- 0.1. Parameters
-- -- Next, we provide additional modelling parameters

-- actionSpace = [1..nActions]

-- -- | Some arbitrary payoff function with a clear max
-- payoffFunction :: State -> Action -> Reward
-- payoffFunction = stepEnvironment

-- -- | Instantiate the single decision with parameters
-- -- We use the _payoff_ function with a peak at 5
-- gameSingleDecisionVerbose = singleDecisionVerbose actionSpace payoffFunction

-- ----------------
-- -- 0.2. Analysis
-- -- We analyze the game and test whether certain actions are optimal
-- -- Here, this is trivial, but it is useful to see it first at that simple level


-- -- | This function defines a checker on the game _gameSingleDecisionVerbose_
-- -- It expects a strategy, here just a single action, as input and outputs information whether the action is optimal
-- -- This is done by the _evaluate_ function. In a game with several players the same _evaluate_ will check for an equilibrium condition
-- -- The _generateisEq_ transforms this output into an easier to digest format.
-- -- NOTE: _void_ represents a context relative to which a game is evaluated. As the game used has
-- -- no incoming or outcoming information, the context is empty (i.e. "void")
-- isOptimalSingleDecisionVerbose strat = generateIsEq $ evaluate gameSingleDecisionVerbose strat void


-- -- | Next, we define a strategu. We use the helper function _pureAction_ to determine a pure strategy
-- -- This is as simples as it gets, the strategy expects an action and will play this
-- -- action with certainty
-- pureIdentity :: Double -> List '[Kleisli Stochastic () Double]
-- pureIdentity action = pureAction action ::- Nil

-- -- | Now, we are ready to actually run this game and the optimality check. Example usages:
-- -- isOptimalSingleDecisionVerbose (pureIdentity 4), or
-- -- isOptimalSingleDecisionVerbose (pureIdentity 5),