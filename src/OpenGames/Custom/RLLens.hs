module OpenGames.Custom.RLLens where
  
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (maximumBy)

import Control.Arrow (Kleisli(..))

import System.Random (randomRIO)

-- import OpenGames.Engine.Engine (Stochastic, certainly)

-- qTableToStrategy :: QTable -> Kleisli Stochastic State Action
-- qTableToStrategy q = Kleisli $ \s -> certainly (chooseGreedy q s)

data RLLens model dmodel obs update = 
    RLLens {
        deploy :: model -> obs -> update ,-- 'forward pass'
        adapt :: model -> (obs, update) -> dmodel -- 'backward pass' 
    }


---------------- Q-learning ------------------

type State = Int
type Action = Int
type Reward = Double
type QTable = Map (State, Action) Double
type Alpha = Double
type Gamma = Double

type Sample = (State, Action, Reward, State) -- observation for training

-- amount of possible actions
nActions :: Int
nActions = 10

-- how greedy the 'model' chooses the best action (low values mean more greediness)
epsilon :: Double
epsilon = 0.1

----------- FUNCTIONS FOR GENERAL Q-Learning SETUP --------------

-- get max possible reward for a sample
computeTarget :: QTable -> Gamma -> Sample -> Double
computeTarget q gamma (s, a, r, s') = 
    let maxNext = maximum [ Map.findWithDefault 0 (s', a') q | a' <- [0..nActions-1] ]
  in r + gamma * maxNext

-- QTable update logic
qUpdate :: Alpha -> QTable -> ((State, Action), Reward) -> QTable
qUpdate alpha q ((s, a), target) = 
    let old = Map.findWithDefault 0 (s, a) q
        new = (1 - alpha) * old + alpha * target
    in Map.insert (s, a) new q

-- deploy: get reward for Q(s, a)
-- adapt: Update QTable from a given sample
qLearningLens :: Alpha -> Gamma -> RLLens QTable QTable Sample Double
qLearningLens alpha gamma = RLLens
  { deploy = \q (s, a, _, _) ->
                Map.findWithDefault 0 (s, a) q -- current Q(s,a)

  , adapt  = \q (sample@(s, a, r, s'), _) ->
                let target = computeTarget q gamma sample
                in qUpdate alpha q ((s, a), target)
  }

--(D A)^S


--------------- FUNCTIONS FOR TRAINING -----------------

-- one update step
simulateStep :: RLLens QTable QTable Sample Double -> QTable -> Sample -> QTable
simulateStep lens q sample =
  let est = deploy lens q sample
  in adapt lens q (sample, est)

testSamples :: [Sample]
testSamples = [ (0, 1, 1.0, 1)
              , (1, 2, 0.5, 2)
              , (2, 3, 1.5, 3)
              ]

train :: Int -> QTable -> [Sample] -> QTable
train 0 q _ = q
train _ q [] = q
train n q (s:ss) =
  let 
    qLens :: RLLens QTable QTable Sample Double
    qLens = qLearningLens 0.1 0.99

    q' = simulateStep qLens q s

    in train (n - 1) q' ss



------------ FUNCTIONS FOR TRAINING ONLINE IN ENVIRONMENT --------------

-- chooses the best action from the QTable greedily
chooseActionGreedy :: QTable -> State -> Action
chooseActionGreedy q s =
  let actions = [0..nActions - 1]
      scored = [(a, Map.findWithDefault 0 (s, a) q) | a <- actions]
    in fst $ maximumBy (\ (_, q1) (_, q2) -> compare q1 q2) scored

-- chooses the best action from the QTable given a certain state based on an epsilon-greedy policy
chooseActionEpsilonGreedy :: QTable -> State -> IO Action     
chooseActionEpsilonGreedy q s = do
  let actions = [0..nActions - 1]
      scored = [(a, Map.findWithDefault 0 (s, a) q) | a <- actions]
  r <- randomRIO(0.0, 1.0)
  if r < epsilon
    then do
      randomIndex <- randomRIO(0, nActions - 1)
      return (actions !! randomIndex)
    else do
      let bestAction = fst $ maximumBy (\ (_, q1) (_, q2) -> compare q1 q2) scored
      return bestAction

-- one step in (dummy) environment
-- returns reward and new state
stepEnvironment :: State -> Action -> (Reward, State)
stepEnvironment s a 
  | s == 0 && a == 1 = (10, 1) 
  | s == 1 && a == 5 = (12, 2) 
  | s == 2 && a == 3 = (6, 3) 
  | s == 3 && a == 7 = (-2, 4) 
  | s == 4 && a == 8 = (-1, 5) 
  | s == 5 && a == 4 = (5, 6) 
  | s == 6 && a == 5 = (1, 0) 
  | otherwise = (-3, 0)


-- one step in the environment
-- takes a 'model', QTable and current state, chooses an action, updates the q values according to the reward, and finally returns the new QTable and state
trainStep :: RLLens QTable QTable Sample Double -> QTable -> State -> IO (QTable, State)
trainStep qLens q s = do
          let a = chooseActionGreedy q s
              (r, s') =  stepEnvironment s a
              sample = (s, a, r, s')
              q' = simulateStep qLens q sample
          return (q', s')

trainStepEpsilonGreedy :: RLLens QTable QTable Sample Double -> QTable -> State -> IO (QTable, State)
trainStepEpsilonGreedy qLens q s = do
            a <- chooseActionEpsilonGreedy q s
            let (r, s') =  stepEnvironment s a
                sample = (s, a, r, s')
                q' = simulateStep qLens q sample
            return (q', s')

-- trains the 'model' for n iterations
trainOnline :: Int -> QTable -> State -> IO (QTable)
trainOnline 0 q s = return q
trainOnline n q s = do
    let qLens = qLearningLens 0.1 0.99
    (q', s') <- trainStep qLens q s
    trainOnline (n - 1) q' s'

trainOnlineEpsilonGreedy :: Int -> QTable -> State -> IO (QTable)
trainOnlineEpsilonGreedy 0 q s = return q
trainOnlineEpsilonGreedy n q s = do
    let qLens = qLearningLens 0.1 0.99
    (q', s') <- trainStepEpsilonGreedy qLens q s
    putStrLn $ " From " ++  show s ++ " to " ++ show s' 
    print q'
    trainOnlineEpsilonGreedy (n - 1) q' s'


testTrain = train 3 Map.empty testSamples
testTrainOnline = trainOnline 100 Map.empty 0

testTrainOnlineEpsilon = trainOnlineEpsilonGreedy 10 Map.empty 0



