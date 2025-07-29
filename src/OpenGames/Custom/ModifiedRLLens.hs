module OpenGames.Custom.ModifiedRLLens where
  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (maximumBy)

import Control.Arrow (Kleisli(..))

import System.Random (randomRIO)
import OpenGames.Custom.RLLens (QTable, State, Action, Reward, computeTarget, qUpdate)
import OpenGames.Engine.BayesianGames (distFromList)

-- Try to match the types etc. with the required interface from the OpenGames library


-- import OpenGames.Engine.Engine (Stochastic, certainly)

-- qTableToStrategy :: QTable -> Kleisli Stochastic State Action
-- qTableToStrategy q = Kleisli $ \s -> certainly (chooseGreedy q s)

alpha = 0.05
gamma = 0.95

type Prob = Double

data QLens qTable state action reward = 
    QLens {
        deploy :: qTable -> state -> [(action, reward)], -- 'forward map' 
        adapt :: qTable -> (state, action, reward, state) -> qTable -- 'backward map' 
    }

-- createProbabilitiesFromRewards :: [(Action, Reward)] -> [(Action, Prob)]
-- createProbabilitiesFromRewards xs = 
--   let maxReward = maximum (map snd xs)
--       maxRewardPairs = filter (\(a, r) -> r == maxReward) xs
--       maxRewardCount = fromIntegral $ length maxRewardPairs
--       otherRewardPairs = filter (\(a, r) -> r /= maxReward) xs
--       otherRewardCount = fromIntegral $ length otherRewardPairs

--       maxRewardDist = map (\(a, r) -> (a, if otherRewardCount == 0 then 1 / maxRewardCount else (1 - alpha) / maxRewardCount)) maxRewardPairs
--       otherRewardDist = [(a, if maxRewardCount == 0 then 1 / otherRewardCount else alpha / otherRewardCount) | (a, r) <- otherRewardPairs]
--   in maxRewardDist ++ otherRewardDist

-- for this to work rewards must be >= 0
createProbabilitiesFromRewards :: [(Action, Reward)] -> [(Action, Prob)]
createProbabilitiesFromRewards xs
  | null xs = []
  | otherwise =
      let maxReward = maximum (map snd xs)
          maxRewardPairs = filter (\(_, r) -> r == maxReward) xs
          otherRewardPairs = filter (\(_, r) -> r /= maxReward) xs
          maxCount = fromIntegral $ length maxRewardPairs
          otherCount = fromIntegral $ length otherRewardPairs

          maxProb = if otherCount == 0 then 1 / maxCount else (1 - alpha) / maxCount
          otherProb = if maxCount == 0 then 1 / otherCount else alpha / otherCount

          dist = [(a, maxProb) | (a, _) <- maxRewardPairs] ++
                 [(a, otherProb) | (a, _) <- otherRewardPairs]

          total = sum (map snd dist)
      in [(a, p / total) | (a, p) <- dist]  -- normalize explicitly


qLearningLensNew :: QLens QTable State Action Reward
qLearningLensNew = QLens 
  {
    deploy = \qTable -> (\s -> createProbabilitiesFromRewards [(a, r) | ((s', a), r) <- Map.toList qTable, s' == s]) -- gets a qTable as argument and returns a function from a state to a distribution of actions for that state
    , 
    adapt = \q sample@(s, a, r, s') ->
                let target = computeTarget q gamma sample
                in qUpdate alpha q ((s, a), target)
  }

testLensForward :: QLens QTable State Action Reward -> QTable -> State -> IO ()
testLensForward lens qTable state = print $ deploy lens qTable state

testLensBackward :: QLens QTable State Action Reward -> QTable -> State -> Action -> Reward -> State -> IO ()
testLensBackward lens qTable state action reward state' = print $ Map.toList $ adapt lens qTable (state, action, reward, state')