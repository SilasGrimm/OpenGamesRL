{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.FPSBA where

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
computePayoffs (playerBids, (v:vs)) = 
  let playerPayoff = computePayoff v playerBids
  in (fst v, playerPayoff) : computePayoffs (playerBids, vs)

computePayoff :: (PlayerName, PlayerValuation) -> [(PlayerName, Bid)] -> Double
computePayoff playerInfo xs = 
    let playerTuple = getPlayerTuple (fst playerInfo) xs
        playerHasMaxBid = snd playerTuple == maximum (map snd xs)
        amountOfMaxBids = countMax xs
    in if playerHasMaxBid then fromIntegral (snd playerInfo - snd playerTuple) / fromIntegral amountOfMaxBids else 0.0

  where getPlayerTuple :: PlayerName -> [(PlayerName, Bid)] -> (PlayerName, Bid)
        getPlayerTuple _ [] = error "Player not found"
        getPlayerTuple player ((p, b):ps) = 
          if p == player then (p, b) else getPlayerTuple player ps
        
        countMax :: Ord b => [(a, b)] -> Int
        countMax [] = 0
        countMax xs =
            let m = maximum (map snd xs)
            in length (filter ((== m) . snd) xs)

getPlayerPayoff :: PlayerName -> [(PlayerName, Double)] -> Double
getPlayerPayoff _ [] = 0
getPlayerPayoff p (x:xs) = if fst x == p then snd x else getPlayerPayoff p xs

maxBid :: Bid
maxBid = 100 -- represent the maximum bidable amount
             -- NOTE: As Q Learning needs a finite state space, we will only consider integer bids (which is reached by making the value space even integers and action space integers)
             --       The value space is only even integers to ensure that the equilibrium bid v/2 (v is valuation) is always an integer

maxVal = maxBid

valueSpace :: [Bid]
valueSpace = [10, 20..maxVal]

actionSpace :: [Bid]
actionSpace = [5, 10..maxBid]

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

runForward
  :: StochasticStatefulOptic s t a b
  -> s
  -> Stochastic a
runForward (StochasticStatefulOptic v _) s = do
  (_z, a) <- v s
  pure a

-- bid of player
biddingStage playerName actionSpace = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentDecision playerName (const actionSpace) ;
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

bidTransform :: ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)], Int) -> ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)])
bidTransform (bs, vs, startIndex) = 
  if startIndex == 1 then (reverse bs, reverse vs) else (bs, vs)

-- Transforms the payments into a random reshuffling
transformBids = [opengame|

    inputs    : (bids, vals) ;
    feedback  :      ;

    :-----------------:
    
    inputs    :    ;
    feedback  :    ;
    operation : nature (uniformDist [0, 1]) ;
    outputs   : startIndex ;
    returns   :  ;
    

    inputs    : (bids, vals, startIndex) ;
    feedback  :      ;
    operation : forwardFunction (bidTransform) ;
    outputs   : transformedBids ;
    returns   :      ;
    :-----------------:

    outputs   : transformedBids ;
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

    inputs    :  player1Value    ;
    feedback  :      ;
    operation :  biddingStage "Player1" actionSpace;
    outputs   :  player1Dec ;
    returns   :  payments  ;

    inputs    :  player2Value    ;
    feedback  :      ;
    operation :  biddingStage "Player2"  actionSpace;
    outputs   :  player2Dec ;
    returns   :  payments  ;

    inputs    :  ([("Player1",player1Dec),("Player2",player2Dec)], [player1Value, player2Value])  ;
    feedback  :      ;
    operation :   computeReturns  ;
    outputs   :  payments ;
    returns   :      ;
    :-----------------:

    outputs   :      ;
    returns   :      ;
|]

initialQTable = Map.fromList [((val, bid), 0) | val <- valueSpace, bid <- actionSpace]

fpsbaLens = qLearningLens' 0.1 0.1 0.8 (const actionSpace) -- gamma is important here because we have a lot of training steps, randomness of opponentAction heavily influences convergence behaviour to Nash Equilibrium during training 
                                                      -- currently we reach the Nash equilibrium with a probability of around 80% - 90% with 2 players, alpha = 0.05, gamma = 0.9, maxBid = 5 and 10000 training steps
fpsbaGreedyLens = qLearningGreedyLens' 0.5 0.95 (const actionSpace)


-- for 2 players, maxBid = 6, alpha = 0.05, gamma = 0.9 and 100000 training steps we reach the Nash Equilibrium with a probability of around 80% - 90%
-- a small increase in state space size makes it much harder to find the Nash Equilibrium -> more training steps, smaller learning rate and smaller epsilon required to reach same probability as with maxBid = 5

trainSteps = 500000

learnFPSBAStrategy :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> IO (QTable Int Int)
learnFPSBAStrategy q lens = do
  learningStep q lens trainSteps -- train epsilon greedy for 350 steps/iterations of the game (since BoS is an one-shot game)
  -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
  -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- plays n games and returns learned strategy
learningStep :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> Int -> IO (QTable Int Int)
learningStep q lens 0 = return q
learningStep q lens n = do
  playerValIndex <- randomRIO (0, length valueSpace - 1)
  -- opponentValIndex <- randomRIO (0, length valueSpace - 1)

  let
    playerVal = valueSpace !! playerValIndex 
    actionDist = deploy' lens q playerVal


  -- opponentAction <- randomRIO (1, maxBid) -- random opponentAction
  opponentValIndex <- randomRIO (0, length valueSpace - 1)
  let 
    opponentVal = valueSpace !! opponentValIndex -- random opponentAction
    opponentAction = div opponentVal 2 -- let opponent bid the nash equilibrium bid


  chosenAction <- sample actionDist -- sample from distribution

  if chosenAction == opponentAction then learningStep q lens (n - 1)
  else 
    let bids = [("Player1", chosenAction), ("Player2", opponentAction)]
        payoff = computePayoff ("Player1", playerVal) bids

    -- putStrLn "-----------------"
    -- putStrLn $ "(Agent Val, Agent Bid): " ++ show (playerVal, chosenAction) ++ " | (OpponentVal, opponentBid): " ++ show (opponentVal, opponentAction)
    -- putStrLn $ "Agent Payoff: " ++ show payoff
    -- putStrLn $ "Iteration: " ++ show (trainSteps - n)
    -- putStrLn $ "Valuation: " ++ show playerVal
    -- putStrLn $ "QTable: " ++ show (toList q)
    -- putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff


        q' = adapt' lens q (playerVal, chosenAction, payoff, Nothing)

    in learningStep q' lens (n - 1)

verifyStrategy :: IO (QTable Int Int) -> QLens' (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int -> IO ()
verifyStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    putStrLn $ "Verified Q: " ++ show (toList q)
    let learnedStrategy = strategyFromLens q lens

    isEquilibriumFPSBACustom (learnedStrategy ::- opponentStrategy ::- Nil)

-- learn a epsilon greedy strategy (configured by bosLens) and then use the learned qTable greedily (configured by bosGreedyLens) to reach the nash equilibrium
checkFPSBAAgent opponentStrategy = verifyStrategy (learnFPSBAStrategy initialQTable fpsbaLens) fpsbaGreedyLens opponentStrategy

checkEquilibriumStrategy strategy = isEquilibriumFPSBACustom (strategy ::- strategy ::- Nil)

strategyFromLens :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int
strategyFromLens q lens = Kleisli $ \(s, v) ->
  distFromList $ deploy' lens q v

opponentAlwaysBid2 :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBid2 = Kleisli $ \(p, v) -> 
  distFromList $ [(2, 1.0)]

opponentAlwaysBid3 :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBid3 = Kleisli $ \(p, v) -> 
  distFromList $ [(3, 1.0)]

-- optimal Strategy
opponentAlwaysBidHalf :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBidHalf = Kleisli $ \(p, v) -> 
  distFromList $ [(div v 2, 1.0)]

opponentAlwaysBidValuation :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
opponentAlwaysBidValuation = Kleisli $ \(p, v) -> 
  distFromList $ [(v , 1.0)]

isEquilibriumFPSBACustom strategyTuple = generateIsEq $ evaluate (firstPriceSealedBidAuction valueSpace actionSpace) strategyTuple void