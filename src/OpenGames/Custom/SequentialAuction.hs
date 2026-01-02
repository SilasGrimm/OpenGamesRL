{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.SequentialAuction where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)
import Examples.Auctions.AuctionSupportFunctions

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import System.Random (randomRIO)

import Data.List (group, sort)

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)
import OpenGames.Custom.SimultaneousAuction_repeated_more_players (PlayerName, PlayerValuation, drawPlayerValuationByNature)

-- This implements a binary valuation first price sequential unit-demand auction

type Bid = Int

-- computePayoffs :: ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)]) -> [(PlayerName, Double)]
-- computePayoffs (_, []) = []
-- computePayoffs (playerBids, (v:vs)) = (fst v, computePayoff v playerBids) : computePayoffs (playerBids, vs)

-- computePayoff :: (PlayerName, PlayerValuation) -> [(PlayerName, Bid)] -> Double
-- computePayoff playerInfo xs = 
--   let playerTuple = getPlayerTuple (fst playerInfo) xs
--       playerHasMaxBid = snd playerTuple == maximum (map snd xs) && snd playerInfo /= 0 -- Also check if valuation unequal to zero
--   in if playerHasMaxBid then fromIntegral (snd playerInfo - snd playerTuple) else 0.0

--   where getPlayerTuple :: PlayerName -> [(PlayerName, Bid)] -> (PlayerName, Bid)
--         getPlayerTuple _ [] = error "Player not found"
--         getPlayerTuple player ((p, b):ps) = 
--           if p == player then (p, b) else getPlayerTuple player ps

computePayoffs :: ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)], Bool) -> [(PlayerName, Double)]
computePayoffs (_, [], _) = []
computePayoffs (playerBids, (v:vs), winnerFound) = 
  let (p, winnerFound') = computePayoff v playerBids winnerFound
  
  in (fst v, p) : computePayoffs (playerBids, vs, winnerFound')

computePayoff :: (PlayerName, PlayerValuation) -> [(PlayerName, Bid)] -> Bool -> (Double, Bool)
computePayoff playerInfo xs winnerFound = 
  if winnerFound then (0.0, True) else 
  let playerTuple = getPlayerTuple (fst playerInfo) xs
      playerHasMaxBid = snd playerTuple == maximum (map snd xs) && snd playerInfo /= 0 -- ensure that previous winners get a reward of zero for the following rounds
  in if playerHasMaxBid then (fromIntegral (snd playerInfo - snd playerTuple), True) else (0.0, False)

  where getPlayerTuple :: PlayerName -> [(PlayerName, Bid)] -> (PlayerName, Bid)
        getPlayerTuple _ [] = error "Player not found"
        getPlayerTuple player ((p, b):ps) = 
          if p == player then (p, b) else getPlayerTuple player ps

getPlayerPayoff :: PlayerName -> [(PlayerName, Double)] -> Double
getPlayerPayoff _ [] = 0
getPlayerPayoff p (x:xs) = if fst x == p then snd x else getPlayerPayoff p xs

numberPlayers :: Int
numberPlayers = 3

numberLots :: Int
numberLots = 2

-- maxVal = 12, valueSpace = [0, 6..maxVal], maxBid = maxVal and actionSpace = [0, 1..maxBid] reaches the Nash equilibrium about 70% of the time
maxVal :: PlayerValuation
maxVal = 3

valueSpace :: [PlayerValuation]
valueSpace = [1..maxVal]

maxBid = maxVal  -- Since b_s(v_i) = v_i * 1 / (n - s + 1) becomes largest when v_i = maxVal and (n - s + 1) = 2, the largest bid that is necessary to reach the equilibrium is maxVal / 2  
                 -- We still keep a larger action space to ensure good learning

actionSpace :: [Bid]
actionSpace = 0 : [1..maxBid]
-- actionSpace = removeDuplicates (0 : [div x 2 | x <- valueSpace] ++ [div x 3 | x <- valueSpace])
  -- where
    -- removeDuplicates :: (Ord a) => [a] -> [a]
    -- removeDuplicates = map head . group . sort

-- OLD value spaces -> This assumes valuations change per round, which they dont
-- getValueSpacePerStage :: Int -> [Valuation]
-- getValueSpacePerStage s = [divisor, 2*divisor..maxVal]
--   where 
--     divisor = numberPlayers - s + 1

-- getValueSpaces :: Int -> [[Valuation]]
-- getValueSpaces 0 = []
-- getValueSpaces n = getValueSpaces (n - 1) ++ [getValueSpacePerStage n]



-- Always gets the first winner (although there may be multiple bids of the same amount)
-- This is our arbitrary rule for the case of ties -> In this case our agent always wins (because he is Player1) 
-- So in case of a tie the Bids b_i of player i have the following relation of importance: b_1 > b_2 > b_3 > b_4
getWinner :: [(PlayerName, Bid)] -> PlayerName
getWinner bids = getWinnerString (maximum $ map snd bids) bids
  where 
    getWinnerString :: Bid -> [(PlayerName, Bid)] -> PlayerName
    getWinnerString maxBid [] = error "Could not determine winner"
    getWinnerString maxBid (b:bs) = if snd b == maxBid then fst b else getWinnerString maxBid bs

getWinnerRandom :: [(PlayerName, Bid)] -> IO PlayerName
getWinnerRandom bids = do
  let maxB = maximum (map snd bids)
      tied = [p | (p, b) <- bids, b == maxB]
  idx <- randomRIO (0, length tied - 1)
  return (tied !! idx)

actionSpaceFunction :: ((PlayerName, PlayerValuation), Int) -> [Bid]
actionSpaceFunction ((n, v), r) = if v == 0 then [0] else actionSpace

-- bid of player
biddingStage name actionSpace = [opengame|

    inputs    :  (nameValuePair, numberOfRemainingBidders)  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  (nameValuePair, numberOfRemainingBidders)  ;
    feedback  :   ;
    operation :  dependentDecision name (\x -> actionSpaceFunction x) ;
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
  inputs    : (bids, valuations, False) ;
  feedback  :      ;
  operation : forwardFunction computePayoffs ;
  outputs   : payments ;
  returns   :      ;
  :-----------------:

  outputs   : payments ;
  returns   :      ;

|]

-- first stage of the sequential auction, this is equal to a FPSBA, except that we now have to get the winner and make sure he can not bet in the next stage
--  -> We do this by setting his valuation to 0 in all upcoming stages
firstStage valSpace actionSpace = [opengame|
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

  inputs    :  (player1Value, numberPlayers)    ;
  feedback  :      ;
  operation :  biddingStage "Player1" actionSpace;
  outputs   :  player1Dec ;
  returns   :  payments  ;

  inputs    :  (player2Value, numberPlayers)    ;
  feedback  :      ;
  operation :  biddingStage "Player2" actionSpace;
  outputs   :  player2Dec ;
  returns   :  endPayments  ;

  inputs    :  (player3Value, numberPlayers)    ;
  feedback  :      ;
  operation :  biddingStage "Player3" actionSpace;
  outputs   :  player3Dec ;
  returns   :  endPayments  ;

  inputs    : [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)] ;
  feedback  :      ;
  operation : liftStochasticForward shuffleBids ;
  outputs   : shuffledBids ;
  returns   :      ;

  inputs    :  (shuffledBids, [player1Value, player2Value, player3Value])  ;
  feedback  :      ;
  operation :   computeReturns  ;
  outputs   :  payments ;
  returns   :      ;
  :-----------------:

  outputs   :   (getWinner shuffledBids, [("Player1",snd player1Value),("Player2",snd player2Value), ("Player3",snd player3Value)], payments)  ;
  returns   :    endPayments  ;
|]

getPlayerValueAfterFirstStage :: PlayerName -> [(PlayerName, PlayerValuation)] -> PlayerValuation
getPlayerValueAfterFirstStage n [] = error "Could not find player"
getPlayerValueAfterFirstStage n (p:ps) = if fst p == n then snd p else getPlayerValueAfterFirstStage n ps



-- similar to first stage, but we now have the last stages winner as the input 
-- make sure lastStageWinner can not bid anymore by using the checkWinnerGame to set his bid to 0
-- set bid to zero in strategy itself whenever valuation is zero

checkLastStageWinner :: (PlayerName, PlayerValuation, PlayerName) -> (PlayerName, PlayerValuation)
checkLastStageWinner (p, v, w) = if p == w then (p, 0) else (p, v)

checkLastStageWinner2 :: (PlayerName, PlayerValuation, (PlayerName, PlayerName)) -> (PlayerName, PlayerValuation)
checkLastStageWinner2 (p, v, (w1, w2)) = if p == w1 || p == w2 then (p, 0) else (p, v)

checkWinner :: (PlayerName, PlayerValuation, [PlayerName]) -> (PlayerName, PlayerValuation)
checkWinner (p, v, []) = (p, v)
checkWinner (p, v, (w:ws)) = if p == w then (p, 0) else checkWinner (p, v, ws)

checkWinnerGame = [opengame|

  inputs    : (playerName, playerValuation, lastStageWinner) ;
  feedback  :      ;

  :-----------------:
  inputs    : (playerName, playerValuation, lastStageWinner) ;
  feedback  :      ;
  operation : forwardFunction checkLastStageWinner ;
  outputs   : val ;
  returns   :      ;
  :-----------------:

  outputs   : val ;
  returns   :      ;

|]

checkWinnerGameEnd = [opengame|

  inputs    : (playerName, playerValuation, lastStageWinner) ;
  feedback  :      ;

  :-----------------:
  inputs    : (playerName, playerValuation, lastStageWinner) ;
  feedback  :      ;
  operation : forwardFunction checkLastStageWinner ;
  outputs   : val ;
  returns   :      ;
  :-----------------:

  outputs   : val ;
  returns   :      ;

|]

-- intermediateStage valSpace actionSpace = [opengame|
--   inputs    :   (lastStageWinner, playerValues)   ;
--   feedback  :      ;

--   :-----------------:

--   inputs    :   ("Player1", getPlayerValueAfterFirstStage "Player1" playerValues, lastStageWinner)   ;
--   feedback  :      ;
--   operation : checkWinnerGame;
--   outputs   :  player1ValueNew ;
--   returns   :      ;

--   inputs    :   ("Player2", getPlayerValueAfterFirstStage "Player2" playerValues, lastStageWinner)   ;
--   feedback  :      ;
--   operation : checkWinnerGame;
--   outputs   :  player2ValueNew ;
--   returns   :      ;

--   inputs    :   ("Player3", getPlayerValueAfterFirstStage "Player3" playerValues, lastStageWinner)   ;
--   feedback  :      ;
--   operation : checkWinnerGame;
--   outputs   :  player3ValueNew ;
--   returns   :      ;

--   inputs    :   ("Player4", getPlayerValueAfterFirstStage "Player4" playerValues, lastStageWinner)   ;
--   feedback  :      ;
--   operation : checkWinnerGame;
--   outputs   :  player4ValueNew ;
--   returns   :      ;

--   inputs    :  (player1ValueNew, numberPlayers - 1, 2)    ;
--   feedback  :      ;
--   operation :  biddingStage "Player1" actionSpace;
--   outputs   :  player1Dec ;
--   returns   :  payments  ;

--   inputs    :  (player2ValueNew, numberPlayers - 1, 2)    ;
--   feedback  :      ;
--   operation :  biddingStage "Player2" actionSpace;
--   outputs   :  player2Dec ;
--   returns   :  payments  ;

--   inputs    :  (player3ValueNew, numberPlayers - 1, 2)    ;
--   feedback  :      ;
--   operation :  biddingStage "Player3" actionSpace;
--   outputs   :  player3Dec ;
--   returns   :  payments  ;

--   inputs    :  (player4ValueNew, numberPlayers - 1, 2)    ;
--   feedback  :      ;
--   operation :  biddingStage "Player4" actionSpace;
--   outputs   :  player4Dec ;
--   returns   :  payments  ;

--   inputs    :  ([("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec), ("Player4",player4Dec)], [player1ValueNew, player2ValueNew, player3ValueNew, player4ValueNew])  ;
--   feedback  :      ;
--   operation :   computeReturns  ;
--   outputs   :  payments ;
--   returns   :      ;
--   :-----------------:

--   outputs   :  getWinner [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec), ("Player4",player4Dec)]  ;
--   returns   :      ;
-- |]

endStage valSpace actionSpace = [opengame|
  inputs    :   (firstStageWinner, playerValues, firstPayments)   ;
  feedback  :    zipWith (\(n, p) (n', p') -> (n, p + p')) firstPayments payments  ;

  :-----------------:

  inputs    :   ("Player1", getPlayerValueAfterFirstStage "Player1" playerValues, firstStageWinner)   ;
  feedback  :      ;
  operation : checkWinnerGameEnd;
  outputs   :  player1ValueNew ;
  returns   :      ;

  inputs    :   ("Player2", getPlayerValueAfterFirstStage "Player2" playerValues, firstStageWinner)   ;
  feedback  :      ;
  operation : checkWinnerGameEnd;
  outputs   :  player2ValueNew ;
  returns   :      ;

  inputs    :   ("Player3", getPlayerValueAfterFirstStage "Player3" playerValues, firstStageWinner)   ;
  feedback  :      ;
  operation : checkWinnerGameEnd;
  outputs   :  player3ValueNew ;
  returns   :      ;

  inputs    :  (player1ValueNew, numberPlayers - 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player1" actionSpace;
  outputs   :  player1Dec ;
  returns   :  zipWith (\(n, p) (n', p') -> (n, p + p')) firstPayments payments   ;

  inputs    :  (player2ValueNew, numberPlayers - 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player2" actionSpace;
  outputs   :  player2Dec ;
  returns   :  zipWith (\(n, p) (n', p') -> (n, p + p')) firstPayments payments  ;

  inputs    :  (player3ValueNew, numberPlayers - 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player3" actionSpace;
  outputs   :  player3Dec ;
  returns   :  zipWith (\(n, p) (n', p') -> (n, p + p')) firstPayments payments  ;

  inputs    : [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)] ;
  feedback  :      ;
  operation : liftStochasticForward shuffleBids ;
  outputs   : shuffledBids ;
  returns   :      ;

  inputs    :  (shuffledBids, [player1ValueNew, player2ValueNew, player3ValueNew])  ;
  feedback  :      ;
  operation :   computeReturns  ;
  outputs   :  payments ;
  returns   :      ;
  :-----------------:

  outputs   :  getWinner shuffledBids  ;
  returns   :      ;
|]


fpsbaSequential valueSpace actionSpace = [opengame|
  inputs   :    ;
  feedback :    ;

  :----------------:
  inputs    :     ;
  feedback  :     ;
  operation : firstStage valueSpace actionSpace ;
  outputs   : (firstLotWinner, playerValues, payments) ;
  returns   : endPayments ;

  inputs    : (firstLotWinner, playerValues, payments) ;
  feedback  : endPayments ;
  operation : endStage valueSpace actionSpace ;
  outputs   : thirdLotWinner ;
  returns   : ;

  :----------------:

  outputs : ;
  returns : ;
|] 

-- initialQTable = Map.fromList $ create numberLots
--   where
--     create 0 = []
--     create n = [(((val, numberPlayers - roundIndex + 1, roundIndex), bid), 0) | val <- getValueSpacePerStage n, roundIndex <- [1..n], bid <- actionSpace]
--                 ++ create (n - 1)
    --            ((valuation, remainingBidders, roundIndex/auctionStage), bid)




initialQTable = Map.fromList $ create numberLots
  where
    create 0 = []
    create n = [(((val, numberPlayers - roundIndex + 1), bid), 0) | val <- valueSpace, roundIndex <- [1..n], bid <- actionSpace]
                ++ create (n - 1)
  
  

-- fpsbaLens = qLearningLens' 0.05 0.01 1.0 (const actionSpace) -- gamma is important here because we have a lot of training steps, randomness of opponentAction heavily influences convergence behaviour to Nash Equilibrium during training 
--                                                       -- currently we reach the Nash equilibrium with a probability of around 60% - 70% with 2 players, alpha = 0.05, gamma = 0.9, maxBid = 5 and 10000 training steps
-- fpsbaGreedyLens = qLearningGreedyLens' 0.5 0.95 (const actionSpace)

-- trainSteps = 1000000

-- learnFPSBAStrategy :: QTable (PlayerValuation, Int) Bid -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> IO (QTable (PlayerValuation, Int) Bid)
-- learnFPSBAStrategy q lens = do
--   learningStep q lens trainSteps -- train epsilon greedy for 350 steps/iterations of the game (since BoS is an one-shot game)
--   -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
--   -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- -- plays n games and returns learned strategy
-- learningStep :: QTable (PlayerValuation, Int) Bid -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> Int -> IO (QTable (PlayerValuation, Int) Bid)
-- learningStep q lens 0 = return q
-- learningStep q lens n = do

--   if mod (trainSteps - n) 10000 == 0 then do
--     putStrLn $ "Iteration: " ++ show (trainSteps - n) ++ " / " ++ show trainSteps
--   else do return ()

--   q' <- playAuction q lens

--   learningStep q' lens (n - 1)

-- playAuction :: QTable (PlayerValuation, Int) Bid -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> IO (QTable (PlayerValuation, Int) Bid)
-- playAuction q lens = do

--   playerValIndex <- randomRIO (0, length valueSpace - 1)
--   opponentValIndex1 <- randomRIO (0, length valueSpace - 1)
--   opponentValIndex2 <- randomRIO (0, length valueSpace - 1)
--   let playerVal = valueSpace !! playerValIndex 
--       opponentVal1 = valueSpace !! opponentValIndex1
--       opponentVal2 = valueSpace !! opponentValIndex2
--   -- opponentVal1 <- getContinuousVal (fromIntegral maxVal)
--   -- opponentVal2 <- getContinuousVal (fromIntegral maxVal)

--   (stage1Winner, q') <- playAuctionStage playerVal [opponentVal1, opponentVal2] q lens 1 []
--   (stage2Winners, q'') <- playAuctionStage playerVal [opponentVal1, opponentVal2] q' lens 2 stage1Winner
--   -- (stage3Winners, q''') <- playAuctionStage playerVal q'' lens 3 stage2Winners
--   return q''

--   where
--     -- Helper to generate a continuous valuation for opponents
--     getContinuousVal :: Double -> IO Double
--     getContinuousVal maxV = randomRIO (0.0, maxV)

--     createBidsArray :: [PlayerName] -> [(PlayerName, Bid)] -> [(PlayerName, Bid)]
--     createBidsArray previousWinners [] = []
--     createBidsArray previousWinners ((n, b):bs) = if elem n previousWinners then (n, 0) : createBidsArray previousWinners bs else (n, b) : createBidsArray previousWinners bs

--     playAuctionStage :: PlayerValuation -> [PlayerValuation] -> QTable (PlayerValuation, Int) Bid -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> Int -> [PlayerName] -> IO ([PlayerName], QTable (PlayerValuation, Int) Bid)
--     playAuctionStage playerVal opponentVals q lens k previousWinners = do
--       -- bidderVals: Array of initial valuations of the bidders, which do not change over the auction stages (except a player has won a round, then they are explicitly set to 0
--       --                because the player should not bid anymore in following rounds)
--       -- q         : The QTable of the agent
--       -- lens      : The QLens of the agent
--       -- k         : The auction stage (1 or 2 since we only have 2 auction lots)
--       -- previousWinners: An Array of winners of previous rounds (used to set the valuation of those winners to zero)

--       let 
--           -- get initial valuation of all players
--           -- playerVal = bidderVals !! 0
--           opponentVal1 = opponentVals !! 0
--           opponentVal2 = opponentVals !! 1

--           -- if a player won a previous round, set their valuation to zero, else keep their initial valuation
--           agentIsPreviousWinner = elem "Player1" previousWinners
--           playerVal' = if agentIsPreviousWinner then 0 else playerVal
--           opponent1IsPreviousWinner = elem "Player2" previousWinners
--           opponentVal1' = if opponent1IsPreviousWinner then 0 else opponentVal1
--           opponent2IsPreviousWinner = elem "Player3" previousWinners
--           opponentVal2' = if opponent2IsPreviousWinner then 0 else opponentVal2

--       let 
--           -- get the action distribution of the agent for its valuation, remaining bidders and auction stage
--         actionDist = deploy' lens q (playerVal', numberPlayers - k + 1)

--         -- opponent1Variance = if k == 1 && opponentVal1' == 6 then -2 else if k == 1 && opponentVal1' == 12 then -1 else 0
--         -- opponent2Variance = if k == 1 && opponentVal2' == 6 then -2 else if k == 1 && opponentVal2' == 12 then -1 else 0
--       let
--       -- choose opponent action according to nash equilibrium bid
--         opponentAction1 = div opponentVal1' (numberPlayers - k + 1)
--         opponentAction2 = div opponentVal2' (numberPlayers - k + 1)

--       -- choose agent action from distribution                            
--       chosenAction <- sample actionDist -- sample from distribution

--       let
--         -- create bids -> This just takes all bids and sets those to zero, which are from previous winners 
--         bids = createBidsArray previousWinners [("Player1", chosenAction), ("Player2", opponentAction1), ("Player3", opponentAction2)]

--       -- putStrLn "-----------------"
--       -- putStrLn $ "Auction stage: " ++ show k 
--       -- putStrLn $ "Valuations and Bids: " ++ show (zip ["Player1", "Player2", "Player3"] (zip [playerVal', opponentVal1', opponentVal2'] (map snd bids)))

--       stageWinner <- getWinnerRandom bids
--       -- if the agent is a previous winner we do not need to train it (since it can not bid in this round)
--       if agentIsPreviousWinner
--         then return (stageWinner : previousWinners, q)
--       else do
      
        
--         let
--           payoff = computePayoff ("Player1", playerVal') bids
--           nextState = if k == numberLots
--                         then Nothing 
--                         else Just (playerVal', numberPlayers - k)


--           q' = adapt' lens q ((playerVal', numberPlayers - k + 1), chosenAction, payoff, nextState)

--         return (stageWinner : previousWinners, q')

--         -- putStrLn $ "QTable: " ++ show (toList q)
--         -- putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

--         -- if elem "Player1" previousWinners then return (stageWinner : previousWinners, q) 
--         -- else 
--         --   if k == numberLots then 
--         --     let q' = adapt' lens q ((playerVal, numberPlayers - k + 1, k), chosenAction, payoff, Nothing)
--         --     in return (stageWinner : previousWinners, q')
--         --   else
--         --     let q' = adapt' lens q ((playerVal, numberPlayers - k + 1, k), chosenAction, payoff, Just (playerVal, numberPlayers - k, k + 1))
--         --     in return (stageWinner : previousWinners, q')

--         -- check if agent won current round
--         --  -> This is necessary because it changes the nextVal in the state update of the player
--         -- let won = stageWinner == "Player1"
--         --     nextVal = if won then 0 else playerVal'

-- verifyStrategy :: IO (QTable (PlayerValuation, Int) Bid) -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> Kleisli Stochastic ((PlayerName, PlayerValuation), Int) Bid -> IO ()
-- verifyStrategy ioQ lens opponentStrategy = do
--     q <- ioQ
--     putStrLn $ "Verified Q: " ++ show (toList q)

--     let learnedStrategy = strategyFromLens q lens

--     isEquilibriumFPSBACustom (learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- Nil)

-- -- learn a epsilon greedy strategy (configured by bosLens) and then use the learned qTable greedily (configured by bosGreedyLens) to reach the nash equilibrium
-- checkFPSBAAgentSequential opponentStrategy = verifyStrategy (learnFPSBAStrategy initialQTable fpsbaLens) fpsbaGreedyLens opponentStrategy

checkEquilibriumStrategy strategy = isEquilibriumFPSBACustom (strategy ::- strategy ::- strategy ::- strategy ::- strategy ::- strategy ::- Nil)

strategyFromLens :: QTable (PlayerValuation, Int) Bid -> QLens' (QTable (PlayerValuation, Int) Bid) (PlayerValuation, Int) Bid Reward -> Kleisli Stochastic ((PlayerName, PlayerValuation), Int) Bid
strategyFromLens q lens = Kleisli $ \((playerName, playerValuation), remainingBidders) ->
  if playerValuation == 0 then distFromList [(0, 1.0)] else distFromList $ deploy' lens q (playerValuation, remainingBidders)

optimalOpponentStrategy :: Kleisli Stochastic ((PlayerName, PlayerValuation), Int) Bid
optimalOpponentStrategy = Kleisli $ \((playerName, playerValuation), remainingBidders) -> 
  -- ensure that the players bid is 0 when his valuation is zero, which can only be the case when the player has already won in a previous auction stage
  --    -> Since the auction is unit-demand, everyone only needs one auction item

  let
    minVal = minimum valueSpace
    bid = if playerValuation == 0 then 0 else minVal + (div (fromIntegral (numberPlayers - numberLots))  (fromIntegral remainingBidders)) * (playerValuation - minVal)
  in distFromList [(bid, 1.0)]
  -- let bid = div (playerValuation * (numberPlayers - numberLots)) remainingBidders
  -- in distFromList [(if bid == 0 && playerValuation /= 0 then bid + 1 else bid, 1.0)]


isEquilibriumFPSBACustom strategyTuple = generateIsEq $ evaluate (fpsbaSequential valueSpace actionSpace) strategyTuple void