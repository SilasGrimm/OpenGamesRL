{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.BoS_repeated where

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

data Player = Player1 | Player2 deriving (Eq, Ord, Show) -- player 1 prefers Bach, player 2 prefers Stravinsky
data ActionBoS = Bach | Stravinsky deriving (Eq, Ord, Show)

bosPayoffMatrix :: Player -> ActionBoS -> ActionBoS -> Reward
bosPayoffMatrix Player1 Bach Bach = 2
bosPayoffMatrix Player1 Bach Stravinsky = 0
bosPayoffMatrix Player1 Stravinsky Bach = 0
bosPayoffMatrix Player1 Stravinsky Stravinsky = 1

bosPayoffMatrix Player2 Bach Bach = 1
bosPayoffMatrix Player2 Bach Stravinsky = 0
bosPayoffMatrix Player2 Stravinsky Bach = 0
bosPayoffMatrix Player2 Stravinsky Stravinsky = 2

bachOrStravinsky = [opengame|
    inputs    :    ;
    feedback  :    ;
    :----------------------------:
    inputs    :    ;
    feedback  :    ;
    operation : dependentDecision "player1" (const [Bach, Stravinsky]) ;
    outputs   : decisionPlayer1 ;
    returns   : bosPayoffMatrix Player1 decisionPlayer1 decisionPlayer2 ;

    inputs    :     ;
    feedback  :     ;
    operation : dependentDecision "player2" (const [Bach, Stravinsky]) ;
    outputs   : decisionPlayer2 ;
    returns   : bosPayoffMatrix Player2 decisionPlayer1 decisionPlayer2 ;
    :----------------------------:
    outputs   :    ;
    returns   :    ;
|]

initialQTable = Map.fromList [((0, Bach), 0), ((0, Stravinsky), 0)]

bosLens = qLearningLens 0.2 0.2 0.95 (const [Bach, Stravinsky])
bosGreedyLens = qLearningGreedyLens 0.5 0.95 (const [Bach, Stravinsky])

trainSteps = 150

learnBoSStrategy :: QTable Int ActionBoS -> QLens (QTable Int ActionBoS) Int ActionBoS Reward -> IO (QTable Int ActionBoS)
learnBoSStrategy q lens = do
  learningStep q lens trainSteps -- train epsilon greedy for 150 steps/iterations of the game (since BoS is an one-shot game)
  -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
  -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- plays n games and returns learned strategy
learningStep :: QTable Int ActionBoS -> QLens (QTable Int ActionBoS) Int ActionBoS Reward -> Int -> IO (QTable Int ActionBoS)
learningStep q lens 0 = return q
learningStep q lens n = do
  let actionDist = deploy lens q 0
      opponentAction = Stravinsky -- The case where to opponent always chooses our preference (Bach) is exactly like the prisoners dilemma, because our reward for choosing Stravinsky is then 0
                                  -- MUST MATCH THE CHOSEN STRATEGY -> TODO: choose based on strategy
  chosenAction <- sample actionDist -- sample from distribution

  let payoff = bosPayoffMatrix Player1 chosenAction opponentAction

  putStrLn $ "Iteration: " ++ show (trainSteps - n)
  putStrLn $ "QTable: " ++ show (toList q)
  putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

  let q' = adapt lens q (0, chosenAction, payoff, 0)

  learningStep q' lens (n - 1)

verifyStrategy :: IO (QTable Int ActionBoS) -> QLens (QTable Int ActionBoS) Int ActionBoS Reward -> Kleisli Stochastic () ActionBoS -> IO ()
verifyStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumBoSCustom (learnedStrategy ::- opponentStrategy ::- Nil)

-- learn a epsilon greedy strategy (configured by bosLens) and then use the learned qTable greedily (configured by bosGreedyLens) to reach the nash equilibrium
checkBoSAgent opponentStrategy = verifyStrategy (learnBoSStrategy initialQTable bosLens) bosGreedyLens opponentStrategy

strategyFromLens :: QTable Int ActionBoS -> QLens (QTable Int ActionBoS) Int ActionBoS Reward -> Kleisli Stochastic () ActionBoS
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ deploy lens q 0

strategy1 :: Kleisli Stochastic () ActionBoS
strategy1 = strategyFromLens initialQTable bosLens

strategy2 :: Kleisli Stochastic () ActionBoS
strategy2 = strategyFromLens initialQTable bosLens

alwaysBach = Kleisli $ \() ->
  distFromList $ [(Stravinsky, 0), (Bach, 1)] -- models a player that prefers to match the interest of the other instead of going to his preferred concert

alwaysStravinsky = Kleisli $ \() ->
  distFromList $ [(Stravinsky, 1), (Bach, 0)] -- models a player that prefers to go to his preferred concert rather than match the interest of the other player

mixedStrategyPreferBach = Kleisli $ \() -> do
  distFromList [(Stravinsky, 1/3), (Bach, 2/3)]

mixedStrategyPreferStravinsky = Kleisli $ \() -> do
  distFromList [(Stravinsky, 2/3), (Bach, 1/3)]

stratTuple = strategy1 ::- strategy1 ::- Nil
stratTuple2 = strategy1 ::- alwaysBach ::- Nil
stratTuple3 = strategy1 ::- alwaysStravinsky ::- Nil

bothBachStrat = alwaysBach ::- alwaysBach ::- Nil
bothStravinskyStrat = alwaysStravinsky ::- alwaysStravinsky ::- Nil
differentStrat = alwaysBach ::- alwaysStravinsky ::- Nil

mixedStrat = mixedStrategyPreferBach ::- mixedStrategyPreferStravinsky ::- Nil

isEquilibriumBoSCustom strategyTuple = generateIsEq $ evaluate bachOrStravinsky strategyTuple void