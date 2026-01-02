{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module OpenGames.Custom.SequentialAuction_repeated where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Engine.BayesianGames (dependentDecision)
import OpenGames.Engine.ExternalEnvironment (extractPayoffAndNextState)

import Data.Map (Map, toList)
import qualified Data.Map as Map
import Control.Arrow (Kleisli (Kleisli))
import Control.Monad.IO.Class (MonadIO(liftIO))

import System.Random (randomRIO)

import Data.List (group, sort)

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)
import OpenGames.Custom.SimultaneousAuction_repeated_more_players (PlayerName, PlayerValuation, drawPlayerValuationByNature)

-- Sequential first price sealed bid auction with k lots (auction stages), n players and unit demand
-- When a player wins one stage, he is not allowed to bid anymore in the upcoming stages -> Therefore the number of players in each auction is constantly reduced by 1
-- The Nash Equilibrium in this case is the bidding function b_s(v_i) = v_i * (n - k) / (n - s + 1) where s is the auction stage, v is the valuation of player i and k is the amount of lots in the auction
-- n > k should hold to make the game interesting (else everyone is going to get a unit of the auctioned product)

-- We use 4 players and 3 auction stages
--    -> Therefore the quilibrium bidding function is b_s(v_i) = v_i * 1 / (n - s + 1) 


-- TODO: 
  -- Change value space to discrete space again for opponents
  -- Adjust open game to the current number of players, lots etc.
  -- Change terminal state update in RLGeneral -> DONE, now update this in every game and check if we still reach the equilibrium
-- NOTES:
  -- No checkWinnerGameBlocks after the biddingStage needed, because we ensure that a players bid is zero inside the strategies themselves in the following games if he has already won

type Valuation = Int
type Bid = Double

computePayoffs :: ([(PlayerName, Bid)], [(PlayerName, PlayerValuation)]) -> [(PlayerName, Double)]
computePayoffs (_, []) = []
computePayoffs (playerBids, (v:vs)) = (fst v, computePayoff v playerBids) : computePayoffs (playerBids, vs)

computePayoff :: (PlayerName, PlayerValuation) -> [(PlayerName, Bid)] -> Double
computePayoff playerInfo xs = 
  let playerTuple = getPlayerTuple (fst playerInfo) xs
      playerHasMaxBid = snd playerTuple == maximum (map snd xs) && snd playerInfo /= 0
  in if playerHasMaxBid then fromIntegral (snd playerInfo) - snd playerTuple else 0.0

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

-- From the setup before
-- maxBid :: Bid
-- maxBid = 8 -- represents the maximum possible valuation of the auction object that players are assigned randomly at the start of the game

-- maxVal = maxBid

-- To ensure that the nash equilibrium bid is an integer, we restrict ourselves to valuations that are divisible by n - s + 1
--    value space should be only one value space, since values are determined once at the start of the game
--    -> Can we convert to the Nash Equilibrium then? -> Values space should only contain values that are divisble by 4, 3 and 2  (n - s + 1 for s in {1, 2, 3})
--    -> Since 4 | x => 2 | x, every possible valuation should be divisible by 4 and 3
--    -> value space is [kgV(3, 4), 2*kgV(3, 4), ...maxVal]

maxVal :: PlayerValuation
maxVal = 6

valueSpace :: [PlayerValuation]
valueSpace = [1, 2..maxVal]

maxBid = maxVal  -- Since b_s(v_i) = v_i * 1 / (n - s + 1) becomes largest when v_i = maxVal and (n - s + 1) = 2, the largest bid that is necessary to reach the equilibrium is maxVal / 2  
                 -- We still keep a larger action space to ensure good learning

actionSpace :: [Bid]
-- actionSpace = [1.0..fromIntegral maxBid]
actionSpace = removeDuplicates ([(fromIntegral x) / 2.0 | x <- valueSpace] ++ [(fromIntegral x) / 3.0 | x <- valueSpace])
  where
    removeDuplicates :: (Ord a) => [a] -> [a]
    removeDuplicates = map head . group . sort

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


-- bid of player
biddingStage name actionSpace = [opengame|

    inputs    :  (nameValuePair, numberOfRemainingBidders, roundIndex)  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  (nameValuePair, numberOfRemainingBidders, roundIndex)  ;
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

  inputs    :  (player1Value, numberPlayers, 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player1" actionSpace;
  outputs   :  player1Dec ;
  returns   :  payments  ;

  inputs    :  (player2Value, numberPlayers, 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player2" actionSpace;
  outputs   :  player2Dec ;
  returns   :  payments  ;

  inputs    :  (player3Value, numberPlayers, 1)    ;
  feedback  :      ;
  operation :  biddingStage "Player3" actionSpace;
  outputs   :  player3Dec ;
  returns   :  payments  ;

  inputs    :  ([("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)], [player1Value, player2Value, player3Value])  ;
  feedback  :      ;
  operation :   computeReturns  ;
  outputs   :  payments ;
  returns   :      ;
  :-----------------:

  outputs   :   (getWinner [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)], [("Player1",snd player1Value),("Player2",snd player2Value), ("Player3",snd player3Value)])  ;
  returns   :      ;
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
  inputs    :   (firstStageWinner, playerValues)   ;
  feedback  :      ;

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

  inputs    :  (player1ValueNew, numberPlayers - 1, 2)    ;
  feedback  :      ;
  operation :  biddingStage "Player1" actionSpace;
  outputs   :  player1Dec ;
  returns   :  payments  ;

  inputs    :  (player2ValueNew, numberPlayers - 1, 2)    ;
  feedback  :      ;
  operation :  biddingStage "Player2" actionSpace;
  outputs   :  player2Dec ;
  returns   :  payments  ;

  inputs    :  (player3ValueNew, numberPlayers - 1, 2)    ;
  feedback  :      ;
  operation :  biddingStage "Player3" actionSpace;
  outputs   :  player3Dec ;
  returns   :  payments  ;

  inputs    :  ([("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)], [player1ValueNew, player2ValueNew, player3ValueNew])  ;
  feedback  :      ;
  operation :   computeReturns  ;
  outputs   :  payments ;
  returns   :      ;
  :-----------------:

  outputs   :  getWinner [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec)]  ;
  returns   :      ;
|]


fpsbaSequential valueSpace actionSpace = [opengame|
  inputs   :    ;
  feedback :    ;

  :----------------:
  inputs    :     ;
  feedback  :     ;
  operation : firstStage valueSpace actionSpace ;
  outputs   : (firstLotWinner, playerValues) ;
  returns:  ;

  inputs    : (firstLotWinner, playerValues) ;
  feedback  : ;
  operation : endStage valueSpace actionSpace ;
  outputs   : secondLotWinner ;
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
    create n = [(((val, numberPlayers - roundIndex + 1, roundIndex), bid), 0) | val <- valueSpace, roundIndex <- [1..n], bid <- actionSpace]
                ++ create (n - 1)
  
  

fpsbaLens = qLearningLens' 0.05 0.01 1.0 (const actionSpace) -- gamma is important here because we have a lot of training steps, randomness of opponentAction heavily influences convergence behaviour to Nash Equilibrium during training 
                                                      -- currently we reach the Nash equilibrium with a probability of around 60% - 70% with 2 players, alpha = 0.05, gamma = 0.9, maxBid = 5 and 10000 training steps
fpsbaGreedyLens = qLearningGreedyLens' 0.5 0.95 (const actionSpace)

trainSteps = 2000000

learnFPSBAStrategy :: QTable (PlayerValuation, Int, Int) Bid -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> IO (QTable (PlayerValuation, Int, Int) Bid)
learnFPSBAStrategy q lens = do
  learningStep q lens trainSteps -- train epsilon greedy for 350 steps/iterations of the game (since BoS is an one-shot game)
  -- whether we reach the equilibrium heavily depends on the first action chosen and on the amount of steps to learn
  -- this is because a wrong/suboptimal first choice still has positive reward, which leads the Q-Learning algorithm to choose this action again with high probability 

-- plays n games and returns learned strategy
learningStep :: QTable (PlayerValuation, Int, Int) Bid -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> Int -> IO (QTable (PlayerValuation, Int, Int) Bid)
learningStep q lens 0 = return q
learningStep q lens n = do

  q' <- playAuction q lens

  learningStep q' lens (n - 1)

playAuction :: QTable (PlayerValuation, Int, Int) Bid -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> IO (QTable (PlayerValuation, Int, Int) Bid)
playAuction q lens = do

  playerValIndex <- randomRIO (1, length valueSpace - 1)
  -- opponentValIndex1 <- randomRIO (1, length valueSpace - 1)
  -- opponentValIndex2 <- randomRIO (1, length valueSpace - 1)
  let playerVal = valueSpace !! playerValIndex 
      -- opponentVal1 = valueSpace !! opponentValIndex1
      -- opponentVal2 = valueSpace !! opponentValIndex2
  opponentVal1 <- getContinuousVal (fromIntegral maxVal)
  opponentVal2 <- getContinuousVal (fromIntegral maxVal)

  (stage1Winner, q') <- playAuctionStage playerVal [opponentVal1, opponentVal2] q lens 1 []
  (stage2Winners, q'') <- playAuctionStage playerVal [opponentVal1, opponentVal2] q' lens 2 stage1Winner
  -- (stage3Winners, q''') <- playAuctionStage playerVal q'' lens 3 stage2Winners
  return q''

  where
    -- Helper to generate a continuous valuation for opponents
    getContinuousVal :: Double -> IO Double
    getContinuousVal maxV = randomRIO (0.0, maxV)

    createBidsArray :: [PlayerName] -> [(PlayerName, Bid)] -> [(PlayerName, Bid)]
    createBidsArray previousWinners [] = []
    createBidsArray previousWinners ((n, b):bs) = if elem n previousWinners then (n, 0.0) : createBidsArray previousWinners bs else (n, b) : createBidsArray previousWinners bs

    playAuctionStage :: PlayerValuation -> [Double] -> QTable (PlayerValuation, Int, Int) Bid -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> Int -> [PlayerName] -> IO ([PlayerName], QTable (PlayerValuation, Int, Int) Bid)
    playAuctionStage playerVal opponentVals q lens k previousWinners = do
      -- bidderVals: Array of initial valuations of the bidders, which do not change over the auction stages (except a player has won a round, then they are explicitly set to 0
      --                because the player should not bid anymore in following rounds)
      -- q         : The QTable of the agent
      -- lens      : The QLens of the agent
      -- k         : The auction stage (1 or 2 since we only have 2 auction lots)
      -- previousWinners: An Array of winners of previous rounds (used to set the valuation of those winners to zero)

      let 
          -- get initial valuation of all players
          -- playerVal = bidderVals !! 0
          opponentVal1 = opponentVals !! 0
          opponentVal2 = opponentVals !! 1

          -- if a player won a previous round, set their validation to zero, else keep their initial validation
          agentIsPreviousWinner = elem "Player1" previousWinners
          playerVal' = if agentIsPreviousWinner then 0 else playerVal
          opponent1IsPreviousWinner = elem "Player2" previousWinners
          opponentVal1' = if opponent1IsPreviousWinner then 0 else opponentVal1
          opponent2IsPreviousWinner = elem "Player3" previousWinners
          opponentVal2' = if opponent2IsPreviousWinner then 0 else opponentVal2

      let 
          -- get the action distribution of the agent for its valuation, remaining bidders and auction stage
        actionDist = deploy' lens q (playerVal', numberPlayers - k + 1, k)

      -- opponent1Variance <- randomRIO (-0.5 :: Double, 0.5 :: Double)
      -- opponent2Variance <- randomRIO (-0.5 :: Double , 0.5 :: Double)
      let
      -- choose opponent action according to nash equilibrium bid
        opponentAction1 = opponentVal1' /  fromIntegral (numberPlayers - k + 1) 
        opponentAction2 = opponentVal2' /  fromIntegral (numberPlayers - k + 1)

      -- choose agent action from distribution                            
      chosenAction <- sample actionDist -- sample from distribution

      let
        -- create bids -> This just takes all bids and sets those to zero, which are from previous winners 
        bids = createBidsArray previousWinners [("Player1", chosenAction), ("Player2", opponentAction1), ("Player3", opponentAction2)]

      -- putStrLn "-----------------"
      -- putStrLn $ "Auction stage: " ++ show k 
      -- putStrLn $ "Valuations and Bids: " ++ show (zip ["Player1", "Player2", "Player3"] (zip [playerVal', opponentVal1', opponentVal2'] (map snd bids)))

      stageWinner <- getWinnerRandom bids
      -- if the agent is a previous winner we do not need to train it (since it can not bid in this round)
      if agentIsPreviousWinner
        then return (stageWinner : previousWinners, q)
      else do
      
        
        let
          payoff = computePayoff ("Player1", playerVal') bids
          nextState = if k == numberLots
                        then Nothing 
                        else Just (if stageWinner == "Player1" then 0 else playerVal', numberPlayers - k, k + 1)


          q' = adapt' lens q ((playerVal', numberPlayers - k + 1, k), chosenAction, payoff, nextState)

        return (stageWinner : previousWinners, q')

        -- putStrLn $ "QTable: " ++ show (toList q)
        -- putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff

        -- if elem "Player1" previousWinners then return (stageWinner : previousWinners, q) 
        -- else 
        --   if k == numberLots then 
        --     let q' = adapt' lens q ((playerVal, numberPlayers - k + 1, k), chosenAction, payoff, Nothing)
        --     in return (stageWinner : previousWinners, q')
        --   else
        --     let q' = adapt' lens q ((playerVal, numberPlayers - k + 1, k), chosenAction, payoff, Just (playerVal, numberPlayers - k, k + 1))
        --     in return (stageWinner : previousWinners, q')

        -- check if agent won current round
        --  -> This is necessary because it changes the nextVal in the state update of the player
        -- let won = stageWinner == "Player1"
        --     nextVal = if won then 0 else playerVal'

        


verifyStrategy :: IO (QTable (PlayerValuation, Int, Int) Bid) -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> Kleisli Stochastic ((PlayerName, PlayerValuation), Int, Int) Bid -> IO ()
verifyStrategy ioQ lens opponentStrategy = do
    q <- ioQ
    putStrLn $ "Verified Q: " ++ show (toList q)

    let learnedStrategy = strategyFromLens q lens

    isEquilibriumFPSBACustom (learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- Nil)

-- learn a epsilon greedy strategy (configured by bosLens) and then use the learned qTable greedily (configured by bosGreedyLens) to reach the nash equilibrium
checkFPSBAAgentSequential opponentStrategy = verifyStrategy (learnFPSBAStrategy initialQTable fpsbaLens) fpsbaGreedyLens opponentStrategy

checkEquilibriumStrategy strategy = isEquilibriumFPSBACustom (strategy ::- strategy ::- strategy ::- strategy ::- strategy ::- strategy ::- Nil)

strategyFromLens :: QTable (PlayerValuation, Int, Int) Bid -> QLens' (QTable (PlayerValuation, Int, Int) Bid) (PlayerValuation, Int, Int) Bid Reward -> Kleisli Stochastic ((PlayerName, PlayerValuation), Int, Int) Bid
strategyFromLens q lens = Kleisli $ \((playerName, playerValuation), remainingBidders, roundIndex) ->
  if playerValuation == 0 then distFromList [(0.0, 1.0)] else distFromList $ deploy' lens q (playerValuation, remainingBidders, roundIndex)

optimalOpponentStrategy :: Kleisli Stochastic ((PlayerName, PlayerValuation), Int, Int) Bid
optimalOpponentStrategy = Kleisli $ \((playerName, playerValuation), remainingBidders, roundIndex) -> 
  -- ensure that the players bid is 0 when his valuation is zero, which can only be the case when the player has already won in a previous auction stage
  --    -> Since the auction is unit-demand, everyone only needs one auction item
  distFromList [(fromIntegral (playerValuation * (numberPlayers - numberLots)) / fromIntegral remainingBidders, 1.0)]

isEquilibriumFPSBACustom strategyTuple = generateIsEq $ evaluate (fpsbaSequential valueSpace actionSpace) strategyTuple void

-- firstPriceSealedBidAuctionInSequentialAuction valSpace actionSpace = [opengame|
--     inputs    :      ;
--     feedback  :      ;

--     :-----------------:
--     inputs    :      ;
--     feedback  :      ;
--     operation : drawPlayerValuationByNature "Player1" valSpace;
--     outputs   :  player1Value ;
--     returns   :      ;


--     inputs    :      ;
--     feedback  :      ;
--     operation : drawPlayerValuationByNature "Player2" valSpace;
--     outputs   :  player2Value ;
--     returns   :      ;

--     inputs    :      ;
--     feedback  :      ;
--     operation : drawPlayerValuationByNature "Player3" valSpace;
--     outputs   :  player3Value ;
--     returns   :      ;

--     inputs    :      ;
--     feedback  :      ;
--     operation : drawPlayerValuationByNature "Player4" valSpace;
--     outputs   :  player4Value ;
--     returns   :      ;

--     inputs    :  player1Value    ;
--     feedback  :      ;
--     operation :  biddingStage "Player1" actionSpace;
--     outputs   :  player1Dec ;
--     returns   :  payments  ;

--     inputs    :  player2Value    ;
--     feedback  :      ;
--     operation :  biddingStage "Player2" actionSpace;
--     outputs   :  player2Dec ;
--     returns   :  payments  ;

--     inputs    :  player3Value    ;
--     feedback  :      ;
--     operation :  biddingStage "Player3" actionSpace;
--     outputs   :  player3Dec ;
--     returns   :  payments  ;

--     inputs    :  player4Value    ;
--     feedback  :      ;
--     operation :  biddingStage "Player4" actionSpace;
--     outputs   :  player4Dec ;
--     returns   :  payments  ;

--     inputs    :  ([("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec), ("Player4",player4Dec)], [player1Value, player2Value, player3Value, player4Value])  ;
--     feedback  :      ;
--     operation :   computeReturns  ;
--     outputs   :  payments ;
--     returns   :      ;
--     :-----------------:

--     outputs   :   getWinner [("Player1",player1Dec),("Player2",player2Dec), ("Player3",player3Dec), ("Player4",player4Dec)]  ;
--     returns   :      ;
-- |]

-- firstPriceSealedBidAuctionSequential valSpace actionSpace = [opengame|
--     inputs    :      ;
--     feedback  :      ;

--     :-----------------:
--     inputs    :      ;
--     feedback  :      ;
--     operation : firstPriceSealedBidAuctionInSequentialAuction valSpace actionSpace;
--     outputs   :  auction1Winner ;
--     returns   :      ;


--     inputs    :      ;
--     feedback  :      ;
--     operation : firstPriceSealedBidAuctionInSequentialAuction valSpace actionSpace;
--     outputs   :  auction2Winner ;
--     returns   :      ;

--     :-----------------:

--     outputs   :      ;
--     returns   :      ;
-- |]

-- initialQTable = Map.fromList [((val, bid), 0) | val <- valueSpace, bid <- actionSpace]

-- fpsbaLens = qLearningLens 0.05 0.05 0.9 (const [0..maxBid]) -- gamma is important here because we have a lot of training steps, randomness of opponentAction heavily influences convergence behaviour to Nash Equilibrium during training 
--                                                       -- currently we reach the Nash equilibrium with a probability of around 60% - 70% with 2 players, alpha = 0.05, gamma = 0.9, maxBid = 5 and 10000 training steps
-- fpsbaGreedyLens = qLearningGreedyLens 0.5 0.95 (const [0..maxBid])

-- trainSteps = 500000

-- learnFPSBAStrategy :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> IO (QTable Int Int)
-- learnFPSBAStrategy q lens = do
--   learningStep q lens trainSteps

-- -- plays n games and returns learned strategy
-- learningStep :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> Int -> IO (QTable Int Int)
-- learningStep q lens 0 = return q
-- learningStep q lens n = do
--   playerValIndex <- randomRIO (0, length valueSpace - 1)
--   -- opponentValIndex <- randomRIO (0, length valueSpace - 1)

--   let
--     playerVal = valueSpace !! playerValIndex 
--     actionDist = deploy lens q playerVal
                                  
--   opponentAction <- randomRIO (1, maxBid) -- random opponentAction
--   chosenAction <- sample actionDist -- sample from distribution

--   let
--     bids = [("Player1", chosenAction), ("Player2", opponentAction)]
--     payoff = computePayoff ("Player1", playerVal) bids

--   -- putStrLn "-----------------"
--   -- putStrLn $ "Iteration: " ++ show (trainSteps - n)
--   -- putStrLn $ "Valuation: " ++ show playerVal
--   -- putStrLn $ "QTable: " ++ show (toList q)
--   -- putStrLn $ "QTable Dist: " ++ show actionDist ++ " | " ++ "Chosen Action: " ++ show chosenAction ++ " | " ++ "Payoff: " ++ show payoff


--   let q' = adapt lens q (playerVal, chosenAction, payoff, playerVal)

--   learningStep q' lens (n - 1)

-- verifyStrategy :: IO (QTable Int Int) -> QLens' (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int -> IO ()
-- verifyStrategy ioQ lens opponentStrategy = do
--     q <- ioQ
--     putStrLn $ "Verified Q: " ++ show (toList q)
--     let learnedStrategy = strategyFromLens q lens

--     isEquilibriumFPSBACustom (learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- opponentStrategy ::- learnedStrategy ::- opponentStrategy ::- opponentStrategy ::- opponentStrategy ::- Nil)

-- -- learn an epsilon greedy strategy (configured by fpsbaLens) and then use the learned qTable greedily (configured by fpsbaGreedyLens) to reach the nash equilibrium
-- checkSimultaneousFPSBAAgent opponentStrategy = verifyStrategy (learnFPSBAStrategy initialQTable fpsbaLens) fpsbaGreedyLens opponentStrategy

-- strategyFromLens :: QTable Int Int -> QLens' (QTable Int Int) Int Int Reward -> Kleisli Stochastic (PlayerName, PlayerValuation) Int
-- strategyFromLens q lens = Kleisli $ \(s, v) ->
--   distFromList $ deploy lens q v

-- opponentAlwaysBid2 :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
-- opponentAlwaysBid2 = Kleisli $ \(p, v) -> 
--   distFromList $ [(2, 1.0)]

-- opponentAlwaysBidHalf :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
-- opponentAlwaysBidHalf = Kleisli $ \(p, v) -> 
--   distFromList $ [(div v 2, 1.0)]

-- opponentAlwaysBidFourth :: Kleisli Stochastic (PlayerName, PlayerValuation) Int
-- opponentAlwaysBidFourth = Kleisli $ \(p, v) -> 
--   distFromList $ [(3 * div v 4, 1.0)]

-- isEquilibriumFPSBACustom strategyTuple = generateIsEq $ evaluate (firstPriceSealedBidAuctionSequential valueSpace actionSpace) strategyTuple void

