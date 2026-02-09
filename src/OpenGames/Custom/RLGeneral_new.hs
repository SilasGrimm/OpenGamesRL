module OpenGames.Custom.RLGeneral_new where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

import Control.Arrow (Kleisli(..))

import System.Random (randomRIO)

-- Type aliases
type Reward = Double
type Prob   = Double
type Alpha  = Double
type Gamma  = Double

-- QTable maps (state, action) to expected reward
type QTable state action = Map (state, action) Reward

type Sample state action = (state, action, Reward, state)

computeTarget :: (Ord state, Ord action)
              => QTable state action
              -> Gamma
              -> (state -> [action])       -- function to get valid actions for a state
              -> Sample state action
              -> Reward
computeTarget q gamma getActions (s, a, r, s') =
    let actions = getActions s'
        maxNext = case actions of
                    [] -> 0
                    _  -> maximum [ Map.findWithDefault 0 (s', a') q | a' <- actions ]
    in r + gamma * maxNext

-- getMaxRewardAction :: (Ord state, Ord action) => QTable state action -> state -> action
-- getMaxRewardAction q s = let 
--     list = Map.toList q
--     filteredList = filter (\((s', a), r) -> s == s') list

--     maxStateActionRewardPair = foldl (\acc ((s, a), r) -> if r > snd acc then ((s, a), r) else acc) (head filteredList) filteredList

--   in snd $ fst maxStateActionRewardPair 


qUpdate :: (Ord state, Ord action)
        => Alpha
        -> QTable state action
        -> ((state, action), Reward)
        -> QTable state action
qUpdate alpha q ((s, a), target) =
    let old = Map.findWithDefault 0 (s, a) q
        new = (1 - alpha) * old + alpha * target
    in Map.insert (s, a) new q

data QLens qTable state action reward =
    QLens
      { deploy :: qTable -> state -> [(action, reward)]  -- forward map
      , adapt  :: qTable -> Sample state action -> qTable -- backward update
      }

qLearningLens :: (Ord state, Ord action)
              => 
              Double
              -> Alpha
              -> Gamma
              -> (state -> [action])   -- function to get valid actions
              -> QLens (QTable state action) state action Reward
qLearningLens epsilon alpha gamma getActions = QLens
  { deploy = \q s ->
                createProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q)
                | a <- getActions s ] epsilon
  , adapt  = \q sample@(s, a, _, _) ->
                let target = computeTarget q gamma getActions sample
                in qUpdate alpha q ((s, a), target)
  }

-- lens only deploys optimal learned strategy
qLearningGreedyLens :: (Ord state, Ord action)
              => Alpha
              -> Gamma
              -> (state -> [action])   -- function to get valid actions
              -> QLens (QTable state action) state action Reward
qLearningGreedyLens alpha gamma getActions = QLens
  { deploy = \q s ->
                createGreedyProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q)
                | a <- getActions s ]
  , adapt  = \q sample@(s, a, _, _) ->
                let target = computeTarget q gamma getActions sample
                in qUpdate alpha q ((s, a), target)
  }

createProbabilitiesFromRewards :: [(action, Reward)] -> Alpha ->  [(action, Prob)]
createProbabilitiesFromRewards xs epsilon
  | null xs = []
  | otherwise =
      let maxReward = maximum (map snd xs)
          maxRewardPairs = filter (\(_, r) -> r == maxReward) xs
          otherRewardPairs = filter (\(_, r) -> r /= maxReward) xs
          maxCount = fromIntegral $ length maxRewardPairs
          otherCount = fromIntegral $ length otherRewardPairs

          maxProb = if otherCount == 0 then 1 / maxCount else (1 - epsilon) / maxCount
          otherProb = if maxCount == 0 then 1 / otherCount else epsilon / otherCount

          dist = [(a, maxProb) | (a, _) <- maxRewardPairs] ++
                 [(a, otherProb) | (a, _) <- otherRewardPairs]

          total = sum (map snd dist)
      in [(a, p / total) | (a, p) <- dist]  -- normalize explicitly

createGreedyProbabilitiesFromRewards :: [(action, Reward)] -> [(action, Prob)]
createGreedyProbabilitiesFromRewards xs 
  | null xs = []
  | otherwise = 
    let maxReward = maximum (map snd xs)
        maxRewardCount = length $ filter (\(_, r) -> r == maxReward) xs 
    in map (\(a, r) -> if r == maxReward then (a, 1 / fromIntegral maxRewardCount) else (a, 0)) xs
    

testLensForward :: QLens (QTable (Int, Int) Int) (Int, Int) Int Reward -> QTable (Int, Int) Int -> (Int, Int) -> IO ()
testLensForward lens qTable state = print $ deploy lens qTable state

testLensBackward :: QLens (QTable (Int, Int) Int) (Int, Int) Int Reward -> QTable (Int, Int) Int -> (Int, Int) -> Int -> Reward -> (Int, Int) -> IO ()
testLensBackward lens qTable state action reward state' = print $ Map.toList $ adapt lens qTable (state, action, reward, state')

alpha = 0.1 -- exploration rate
gamma = 0.95 -- how valuable newer experiences are over old ones

qlens = qLearningLens alpha alpha gamma

qTable :: QTable (Int, Int) Int
qTable = Map.fromList [(((0, 0), 0), 0), (((0, 1), 0), 0), (((1, 0), 0), 0), (((1, 1), 0), 0),
                       (((0, 0), 1), 0), (((0, 1), 1), 0), (((1, 0), 1), 0), (((1, 1), 1), 0)]

-- test forward deployment
testForward q = deploy (qlens (\s -> [0, 1])) q (1, 0)

testBackward = adapt (qlens (\s -> [0, 1])) qTable ((1, 0), 1, 5, (1, 0))

sample :: [(action, Reward)] -> IO action
sample dist = do
    let cumulative = scanl1 (\(_, acc) (a, p) -> (a, acc + p)) dist
    r <- randomRIO (0, 1)  -- generate random Double in [0,1]
    return $ fst $ head $ dropWhile (\(_, p) -> p < r) cumulative



----------------------------------------

-- This small adjustment allows for correct handling of terminal states
-- Should be used in every game, so that the last state transition can be handled correctly
type Sample' state action = (state, action, Reward, Maybe state)

computeTarget' :: (Ord state, Ord action)
              => QTable state action
              -> Gamma
              -> (state -> [action])       -- function to get valid actions for a state
              -> Sample' state action
              -> Reward
computeTarget' q gamma getActions (s, a, r, Nothing) = r
computeTarget' q gamma getActions (s, a, r, Just s') =
    let actions = getActions s'
        maxNext = if null actions
                    then 0
                    else maximum [ Map.findWithDefault 0 (s', a') q | a' <- actions ]
    in r + gamma * maxNext

-- getMaxRewardAction :: (Ord state, Ord action) => QTable state action -> state -> action
-- getMaxRewardAction q s = let 
--     list = Map.toList q
--     filteredList = filter (\((s', a), r) -> s == s') list

--     maxStateActionRewardPair = foldl (\acc ((s, a), r) -> if r > snd acc then ((s, a), r) else acc) (head filteredList) filteredList

--   in snd $ fst maxStateActionRewardPair 

data QLensNew qTable state action reward =
    QLensNew
      { deploy' :: (qTable, state) -> [(action, Prob)]  -- forward map
      , adapt'  :: (qTable, state) -> Sample' state action -> qTable -- backward update
      }

-- qLearningLens' :: (Ord state, Ord action)
--               => 
--               Double
--               -> Alpha
--               -> Gamma
--               -> (state -> [action])   -- function to get valid actions
--               -> QLensNew (QTable state action) state action Reward
-- qLearningLens' epsilon alpha gamma getActions = QLensNew
--   { deploy' = \(q, s) ->
--                 createProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q)
--                 | a <- getActions s ] epsilon
--   , adapt'  = \(q, sample@(s, a, _, _)) ->
--                 let target = computeTarget' q gamma getActions sample
--                 in qUpdate alpha q ((s, a), target)
--   }

qLearningLens' :: (Ord state, Ord action)
              => 
              Double
              -> Alpha
              -> Gamma
              -> (state -> [action])   -- function to get valid actions
              -> QLensNew (QTable state action) state action Reward
qLearningLens' epsilon alpha gamma getActions = QLensNew
  { deploy' = \(q, s) ->
                -- let actions = getActions s
                --     in maximumBy
                --         (comparing (\a -> Map.findWithDefault 0 (s, a) q))
                --         actions
                createProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q)
                | a <- getActions s ] epsilon

  , adapt'  = \(q, _) sample@(s, a, _, _) ->
                let target = computeTarget' q gamma getActions sample
                in qUpdate alpha q ((s, a), target)
  }

  -- lens only deploys optimal learned strategy
-- qLearningGreedyLens' :: (Ord state, Ord action)
--               => Alpha
--               -> Gamma
--               -> (state -> [action])   -- function to get valid actions
--               -> QLensNew (QTable state action) state action Reward
-- qLearningGreedyLens' alpha gamma getActions = QLensNew
--   { deploy' = \(q, s) ->
--                 createGreedyProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q)
--                 | a <- getActions s ]
--   , adapt'  = \(q, _) sample@(s, a, _, _) ->
--                 let target = computeTarget' q gamma getActions sample
--                 in qUpdate alpha q ((s, a), target)
--   }