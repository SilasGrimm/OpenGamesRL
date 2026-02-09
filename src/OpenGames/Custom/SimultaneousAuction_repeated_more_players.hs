{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.SimultaneousAuction_repeated_more_players where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Engine.BayesianGames (dependentDecision)
import Examples.Auctions.AuctionSupportFunctions
import Data.Map (Map, toList)
import qualified Data.Map as Map

import System.Random (randomRIO)

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

type PlayerName = String
type Bid = Int -- The players bid
type PlayerValuation = Int -- The players internal value of the auction object


computePayoffs :: ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)]) -> [(PlayerName, Double)]
computePayoffs (_, []) = []
computePayoffs (playerBids, (v:vs)) = (fst v, computePayoff v playerBids) : computePayoffs (playerBids, vs)

computePayoff :: (PlayerName, PlayerValuation) -> [(PlayerName, Bid)] -> Double
computePayoff playerInfo xs = 
  let playerTuple = getPlayerTuple (fst playerInfo) xs
      playerHasMaxBid = snd playerTuple == maximum (map snd xs)
  in if playerHasMaxBid then fromIntegral $ snd playerInfo - snd playerTuple else 0.0

  where getPlayerTuple :: PlayerName -> [(PlayerName, Bid)] -> (PlayerName, Bid)
        getPlayerTuple _ [] = error "Player not found"
        getPlayerTuple player ((p, b):ps) = 
          if p == player then (p, b) else getPlayerTuple player ps

getPlayerPayoff :: PlayerName -> [(PlayerName, Double)] -> Double
getPlayerPayoff _ [] = 0
getPlayerPayoff p (x:xs) = if fst x == p then snd x else getPlayerPayoff p xs

maxBid :: Bid
maxBid = 12 -- represent the maximum bidable amount
             -- NOTE: As Q Learning needs a finite state space, we will only consider integer bids (which is achieved by making the value space integers divisible by the number of players n and action space integers)
             --       The value space is only even integers to ensure that the equilibrium bid (n-1) * v/n (v is valuation) is always an integer
             --       Note that the agent essentially only observes his own valuation. Observing the amount of players is unneccessary, because this is implicitly given in the environment payoffs

maxVal = maxBid

valueSpace :: [Bid] -- must be multiple of the number of players
valueSpace = [4, 8..maxVal]

actionSpace :: [Bid]
actionSpace = [1..maxBid]

-- game that assigns a player a valuation of the auction object randomly
drawPlayerValuationByNature playerName valueSpace = [opengame|
    inputs    :    ;
    feedback  :    ;
    :----------------------------:
    inputs    :    ;
    feedback  :    ;
    operation : nature (uniformDist valueSpace) ;
    outputs   : val ;
    returns   :  ;
    :----------------------------:
    outputs   :  (playerName, val)  ;
    returns   :    ;
|]

-- bid of player
biddingStage name actionSpace = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentDecision name (const actionSpace) ;
    outputs   :  bid ;
    returns   :  getPlayerPayoff (fst nameValuePair) payments  ;
    :---------------------------:

    outputs   :  bid ;
    returns   :  payments  ;
  |]

computeReturns = [opengame|

  inputs    : (bids, valuations) ;
  feedback  :      ;

  :-----------------:
  inputs    : (bids, valuations) ;
  feedback  :      ;
  operation : forwardFunction computePayoffs ;
  outputs   : payments ;
  returns   :      ;
  :-----------------:

  outputs   : payments ;
  returns   :      ;

|]


firstPriceSealedBidAuction valSpace actionSpace = [opengame|
    inputs    :      ;
    feedback  :      ;

    :-----------------:
    inputs    :      ;
    feedback  :      ;
    operation : drawPlayerValuationByNature "Player1" valSpace;
    outputs   :  player1Value ;
    returns   :      ;


    inputs    :      ;
    feedback  :      ;
    operation : drawPlayerValuationByNature "Player2" valSpace;
    outputs   :  player2Value ;
    returns   :      ;

    inputs    :      ;
    feedback  :      ;
    operation : drawPlayerValuationByNature "Player3" valSpace;
    outputs   :  player3Value ;
    returns   :      ;

    inputs    :      ;
    feedback  :      ;
    operation : drawPlayerValuationByNature "Player4" valSpace;
    outputs   :  player4Value ;
    returns   :      ;

    inputs    :  player1Value    ;
    feedback  :      ;
    operation :  biddingStage "Player1" actionSpace;
    outputs   :  player1Dec ;
    returns   :  payments  ;

    inputs    :  player2Value    ;
    feedback  :      ;
    operation :  biddingStage "Player2" actionSpace;
    outputs   :  player2Dec ;
    returns   :  payments  ;

    inputs    :  player3Value    ;
    feedback  :      ;
    operation :  biddingStage "Player3" actionSpace;
    outputs   :  player3Dec ;
    returns   :  payments  ;

    inputs    :  player4Value    ;
    feedback  :      ;
    operation :  biddingStage "Player4" actionSpace;
    outputs   :  player4Dec ;
    returns   :  payments  ;

    inputs    :  ([("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec), ("Player4",player4Dec)], [player1Value, player2Value, player3Value, player4Value])  ;
    feedback  :      ;
    operation :   computeReturns  ;
    outputs   :  payments ;
    returns   :      ;
    :-----------------:

    outputs   :      ;
    returns   :      ;
|]

initialQTable = Map.fromList [((val, bid), 0) | val <- valueSpace, bid <- actionSpace]

fpsbaLens = qLearningLens 0.05 0.05 0.9 (const actionSpace) -- gamma is important here because we have a lot of training steps, randomness of opponentAction heavily influences convergence behaviour to Nash Equilibrium during training 
                                                      -- currently we reach the Nash equilibrium with a probability of around 60% - 70% with 2 players, alpha = 0.05, gamma = 0.9, maxBid = 5 and 10000 training steps
fpsbaGreedyLens = qLearningGreedyLens 0.5 0.95 (const actionSpace)


-- TODO: try again
-- for 3 players, maxBid = 6, alpha = epsilon = 0.05, gamma = 0.9 and trainSteps = 500000 we reach the nash equilibrium with a probability of roughly 90%
-- for 3 players, maxBid = 9, alpha = epsilon = 0.05, gamma = 0.9 and trainSteps = 500000 we reach the nash equilibrium with a probability of roughly 50%

-- for 4 players, maxBid = 8, alpha = epsilon = 0.05, gamma = 0.9 and trainSteps = 500000 we reach the nash equilibrium with a probability of roughly 90%
-- for 4 players, maxBid = 12, alpha = epsilon = 0.05, gamma = 0.9 and trainSteps = 500000 we reach the nash equilibrium with a probability of roughly 80% - 90%

trainSteps = 500000

learnFPSBAStrategy :: QTable Int Int -> QLens (QTable Int Int) Int Int Reward -> IO (QTable Int Int)
learnFPSBAStrategy q lens = do
  learningStep q lens trainSteps -- train epsilon greedy for 350 steps/iterations of the game (since BoS is an one-shot game)
  -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
  -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- plays n games and returns learned strategy
learningStep :: QTable Int Int -> QLens (QTable Int Int) Int Int Reward -> Int -> IO (QTable Int Int)
learningStep q lens 0 = return q
learningStep q lens n = do
  playerValIndex <- randomRIO (0, length valueSpace - 1)
  -- opponentValIndex <- randomRIO (0, length valueSpace - 1)

  let
    playerVal = valueSpace !! playerValIndex 
    actionDist = deploy lens q playerVal
                                  
  -- opponentAction1 <- randomRIO (1, maxBid) -- random opponentAction
  -- opponentAction2 <- randomRIO (1, maxBid) -- random opponentAction
  -- opponentAction3 <- randomRIO (1, maxBid) -- random opponentAction
  opponentValIndex1 <- randomRIO (0, length valueSpace - 1)
  opponentValIndex2 <- randomRIO (0, length valueSpace - 1)
  opponentValIndex3 <- randomRIO (0, length valueSpace - 1)
  let 
    opponentVal1 = valueSpace !! opponentValIndex1 -- random opponentAction
    opponentVal2 = valueSpace !! opponentValIndex2 -- random opponentAction
    opponentVal3 = valueSpace !! opponentValIndex3 -- random opponentAction
    -- let opponents bid the nash equilibrium bid
    opponentAction1 = 3 * div opponentVal1 4
    opponentAction2 = 3 * div opponentVal2 4
    opponentAction3 = 3 * div opponentVal3 4


  chosenAction <- sample actionDist -- sample from distribution

  let
    bids = [("Player1", chosenAction), ("Player2", opponentAction1), ("Player3", opponentAction2), ("Player4", opponentAction3)]
    payoff = computePayoff ("Player1", playerVal) bids

  -- putStrLn "-----------------"
  -- putStrLn $ "Iteration: " ++ show (trainSteps - n)
  -- putStrLn $ "Valuation: " ++ show playerVal
  -- putStrLn $ "QTable: " ++ show (toList q)
  -- putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff


  let q' = adapt lens q (playerVal, chosenAction, payoff, playerVal)

  learningStep q' lens (n - 1)

verifyStrategy :: IO (QTable Int Int) -> QLens (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int -> IO ()
verifyStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    putStrLn $ "Verified Q: " ++ show (toList q)
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumFPSBACustom (learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- opponentStrategy ::- Nil)

-- learn a epsilon greedy strategy (configured by bosLens) and then use the learned qTable greedily (configured by bosGreedyLens) to reach the nash equilibrium
checkFPSBAAgentMoreThanTwo opponentStrategy = verifyStrategy (learnFPSBAStrategy initialQTable fpsbaLens) fpsbaGreedyLens opponentStrategy

checkEquilibriumStrategy strategy = isEquilibriumFPSBACustom (strategy ::- strategy ::- strategy ::- strategy ::- Nil)

strategyFromLens :: QTable Int Int -> QLens (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int
strategyFromLens q lens = Kleisli $ \(s, v) ->
  distFromList $ deploy lens q v

opponentAlwaysBid2 :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBid2 = Kleisli $ \(p, v) -> 
  distFromList $ [(2, 1.0)]

opponentAlwaysBidThird :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBidThird = Kleisli $ \(p, v) -> 
  distFromList $ [(2 * div v 3, 1.0)]

-- optimal Strategy
opponentAlwaysBidFourth :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBidFourth = Kleisli $ \(p, v) -> 
  distFromList $ [(3 * div v 4, 1.0)]

isEquilibriumFPSBACustom strategyTuple = generateIsEq $ evaluate (firstPriceSealedBidAuction valueSpace actionSpace) strategyTuple void