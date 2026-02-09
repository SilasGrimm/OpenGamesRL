{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module OpenGames.Custom.PD_compositional where

import Control.Monad
import Control.Lens
import Control.Monad.State

import OpenGames.Custom.RLGeneral hiding (sample)

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef

import OpenGames.Custom.PrisonersDilemma_new

import System.Random

class RightModule m f where
  act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
  act = join
 
instance RightModule m (Const a) where
  act = Const . getConst

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicLens :: forall m f s t a b . (Functor f, RightModule m f) 
  => (s -> a) -> (s -> b -> m t) -> LensLike f s t a b
monadicLens v u k s = act (fmap (u s) (k (v s)))

-- this lens just inserts the updated value into the map
-- in all functions, i is a key of type (state, action)
update :: Ord i => MonadicLens IO (IORef (Map i Double)) () (i -> IO Double) (i, Double)
update = monadicLens @IO read write
  where
    read ref i = do
      m <- readIORef ref
      pure $ Map.findWithDefault 0 i m

    write ref (i, x) =
      modifyIORef' ref (Map.insert i x)

-- f is of type (i -> Double), (i, x) is of type (i, Double)
-- f gets the current value at index i and sums it with x, the output of the previous step (the bellman target)
-- this completes the Q update function
learningRate :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double)
learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {y <- f i; pure (i, alpha*x + (1 - alpha)*y)}


-- this computes the Q-Learning target based on (s, a, r, s)
-- f is a function that gets the q-value at a specific array index, j is the array index of the maximum possible state-action pair for a given state, i is the array index of the state-action pair to update 
-- x is the reward
bellman :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double, i)
bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do {y <- f j; pure (i, x + discountFactor*y)}

backwardMap :: Ord i => Double -> Double -> MonadicLens IO (IORef (Map i Double)) () (i -> IO Double) (i, Double, i)
-- update . learningRate alpha . bellman gamma is equal to the block G' in the string diagram of rl cybernetics
backwardMap alpha gamma =
    update
  . learningRate alpha
  . bellman gamma

forwardMap :: (Ord state, Num state) => MonadicLens IO (IORef (Map (state, Action) Double)) () (state -> IO Action) ()
forwardMap = monadicLens @IO (epsilonGreedy 0.1) (\qRef () -> pure ())



readQ :: (Ord state) => IORef (Map (state, Action) Double) -> (state, Action) -> IO Double
readQ ref i = do
      m <- readIORef ref
      pure $ Map.findWithDefault 0 i m

-- Q lookup from lens
qRead :: (Ord state) => IORef (Map (state, Action) Double)
      -> (state, Action)
      -> IO Double
qRead = readQ

-- policy :: Double -> ((state, Action) -> IO Double) -> state -> IO Action
-- policy eps q s = 

-- Policy uses Q lookup
epsilonGreedy' :: Double -> ((state, Action) -> IO Double) -> state -> IO Action
epsilonGreedy' epsilon q s = do
  r <- randomRIO (0.0, 1.0)
  if r < epsilon
    then randomAction
    else greedyAction
  where
    actions = [Testify, StaySilent]

    randomAction =
      (actions !!) <$> randomRIO (0, length actions - 1)

    greedyAction = do
      qs <- mapM (\a -> q (s, a)) actions
      let (_, best) = maximum (zip qs actions)
      pure best

argmaxAction :: ((state, Action) -> IO Double) -> state -> IO Action
argmaxAction q s = do
  let actions = [Testify, StaySilent]
  qs <- mapM (\a -> q (s, a)) actions
  let (_, best) = maximum (zip qs actions)
  pure best

runBackward
  :: MonadicLens IO s t a b
  -> s
  -> (a -> IO b)
  -> IO t
runBackward l s k =
  l k s

type Env state action =
  state -> action -> IO (Double, state)

step
  :: (Ord state) => IORef (Map (state, Action) Double)
  -> state
  -> (state -> Action -> IO (Double, state))  -- environment
  -> Double                                   -- epsilon
  -> Double                                   -- alpha
  -> Double                                   -- gamma
  -> IO state
step qRef s env eps alpha gamma = do
  let q = readQ qRef  -- forward wire

  -- pick action using the compositional policy
  a <- epsilonGreedy' eps q s

  -- interact with the environment
  (r, s') <- env s a

  -- find best next action for Bellman update
  a' <- argmaxAction q s'

  -- update Q-table via backward lens
  runBackward (backwardMap alpha gamma) qRef (\_ -> pure ((s, a), r, (s', a')))

  pure s'

-- qLensComp :: (Ord state, Num state) => MonadicLens IO (IORef (Map (state, Action) Double)) () (state -> IO Action) (state, Action, Double, state)
-- qLensComp = monadicLens @IO qLensForward qLensBackward

-- qLensForward :: (Ord state, Num state) => IORef (Map (state, Action) Double) -> state -> IO Action
-- qLensForward = epsilonGreedy 0.1

-- qLensBackward qRef (s, a, r, s') = do 
--   let q = readQ qRef; 
--   currentVal <- q (s, a)
--   argmax <- getMaximumNextState q 0
--   let target = r + 0.8 * argmax
--       newVal = (1 - 0.1) * currentVal + 0.1 * target 
--   modifyIORef' qRef (Map.insert (s, a) newVal)

-- getMaximumNextState :: ((state, Action) -> IO Double) -> state -> IO Double
-- getMaximumNextState q s = do
--   qs <- mapM (\a -> q (s, a)) [Testify, StaySilent]
--   pure (maximum qs)


-- runForward
--   :: MonadicLens IO s t a b
--   -> s
--   -> a
-- runForward l s =
--   getConst (l Const s)

-- runBackward
--   :: MonadicLens IO s t a b
--   -> s
--   -> b
--   -> t
-- runBackward l s b =
--   runIdentity (l (\_ -> Identity b) s)

-- trainStep qRef env alpha gamma s = do
--   let policy = runForward forwardMap qRef

--   a <- policy s
--   (s', r) <- env s a

--   aMax <- greedy (\(st, ac) -> readQ qRef (st, ac)) s'

--   let () =
--         runBackward
--           (backwardMap alpha gamma)
--           qRef
--           ((s, a), r, (s', aMax))

--   pure s'

-- trainEpisode
--   :: Int
--   -> state
--   -> (state -> IO state)
--   -> IO ()
-- trainEpisode 0 _ _ = pure ()
-- trainEpisode n s stepFn = do
--   s' <- stepFn s
--   trainEpisode (n - 1) s' stepFn

-- qLearningPD = do
--   qRef <- newIORef Map.empty

--   let stepFn =
--         trainStep
--           qRef
--           (\s a -> pure (pdRewards s a))
--           0.1   -- alpha
--           0.95  -- gamma

--   trainEpisode 10000 0 stepFn
  







sample :: (state, [(action, Prob)]) -> IO action
sample (_, xs) = do
  r <- randomRIO (0, 1)
  pure (go r xs)
  where
    go _ [] = error "empty distribution"
    go p ((a,w):rest)
      | p <= w    = a
      | otherwise = go (p - w) rest

-- agent :: Double -> MonadicLens IO (i -> IO Double) (state, action, Double, state) (i -> IO Double) (action, state, Double)
-- agent epsilon = monadicLens @IO id (\(s, dist) (a, s', r) -> pure (s, a, r, s'))
agent :: Double -> MonadicLens IO (state, state -> IO Action) (state, Action, Double, state) (state, IO Action) (Action, state, Double)
agent epsilon = monadicLens @IO (\(s, f) -> (s, f s)) (\(s, dist) (a, s', r) -> pure (s, a, r, s'))

-- this lens takes a function f as its argument, which should return the next state and payoff, given the current state and the chosen action
-- the forward pass of this lens is from (state, action) to (), the backward pass is from () to (state, Double)
-- essentially, the forward pass does nothing and the backward pass uses f to return the next state and current payoff
environment :: (state -> action -> IO (state, Double)) -> MonadicLens IO (state, action) (state, Double) () ()
environment f = monadicLens @IO (const ()) (const . uncurry f)

-- we sample the action in the environment lens
-- environment' :: (state -> action -> (state, Double)) -> MonadicLens IO (state, [(action, Prob)]) (action, state, Double) () ()
-- environment' f = monadicLens @IO (const ()) (\(s, dist) () -> do {a <- sample (s, dist); let (s, r) = f s a in pure (a, s, r)})
environment' :: (state -> action -> (state, Double)) -> MonadicLens IO (state, IO action) (action, state, Double) () ()
environment' f = monadicLens @IO (const ()) (\(s, ioa) () -> do {a <- ioa; let (s, r) = f s a in pure (a, s, r)})

-- state is always 0 for prisoners dilemma, opponent always testifies
pdRewards :: state -> Action -> (state, Double)
pdRewards s a = (s, prisonersDilemmaMatrix a Testify)

-- step
--   :: state
--   -> IORef (Map (state, Action) Double)
--   -> IO state
-- step s qRef = do
--   -- 1. compute policy from Q
--   dist <- epsilonGreedy qRef s 0.1

--   -- 2. run ONE learning step via the lens pipeline
--   let pipeline = update . learningRate alpha . bellman gamma . agent . environment' pdRewards
    
--   (_, s', _, _) <- pipeline (\_ -> Identity ()) (s, dist)

--   pure s'

greedy :: (Enum action) => ((state, action) -> IO Double) -> state -> IO action
greedy = undefined


epsilonGreedy :: (Ord state, Num state) => Double -> IORef (Map (state, Action) Double) -> (state -> IO Action)
epsilonGreedy epsilon qRef = policy where 
  policy s = do 
    q <- readIORef qRef
    let qs = createProbabilitiesFromRewards [ (a, Map.findWithDefault 0 (s, a) q) | a <- [Testify, StaySilent] ] epsilon
    randIndex <- randomRIO (0, length qs - 1)
    sample (s, qs)


-- import Control.Monad
-- import Control.Lens
-- import Data.Array.IO
-- import Data.Ix
-- import Data.Functor.Const

-- -- RightModule definition
-- class RightModule m f where
--   act :: f (m a) -> f a

-- instance Monad m => RightModule m m where
--   act = join

-- instance RightModule m (Const a) where
--   act = Const . getConst

-- -- MonadicLens type
-- type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

-- -- Helper to build a monadic lens
-- monadicLens :: forall m f s t a b. (Functor f, RightModule m f)
--             => (s -> a)
--             -> (s -> b -> m t)
--             -> LensLike f s t a b
-- monadicLens v u k s = act (fmap (u s) (k (v s)))

-- -- Prisoner's Dilemma definitions
-- type State = Int
-- data Action = Testify | StaySilent
--   deriving (Eq, Ord, Show, Enum, Bounded)

-- type Reward = Double

-- prisonersDilemmaMatrix :: Action -> Action -> Reward
-- prisonersDilemmaMatrix StaySilent StaySilent = -1
-- prisonersDilemmaMatrix StaySilent Testify    = -3
-- prisonersDilemmaMatrix Testify    StaySilent = 0
-- prisonersDilemmaMatrix Testify    Testify    = -2

-- -- Ix instance for Action (needed for IOUArray)
-- instance Ix Action where
--   range (l,u) = enumFromTo l u
--   index (l,_) a = fromEnum a - fromEnum l
--   inRange (l,u) a = a >= l && a <= u

-- -- Q-table bounds
-- qBounds :: ((State, Action), (State, Action))
-- qBounds = ((0, StaySilent), (0, Testify))

-- -- Monadic lenses for Q-learning
-- update :: MonadicLens IO (IOUArray (State, Action) Double) () ((State, Action) -> IO Double) ((State, Action), Double)
-- update = monadicLens @IO readArray (uncurry . writeArray)

-- -- learningRate :: Double -> MonadicLens IO ((State, Action) -> IO Double) (IOUArray (State, Action) Double) ((State, Action) -> IO Double) ((State, Action), Double)
-- -- learningRate alpha = monadicLens @IO id $ \f (i, x) -> do
-- --   y <- f i
-- --   pure (i, alpha*x + (1 - alpha)*y)

-- -- bellman :: Double -> MonadicLens IO ((State, Action) -> IO Double) (IOUArray (State, Action) Double) ((State, Action) -> IO Double) ((State, Action), Double, (State, Action))
-- -- bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do
-- --   y <- f j
-- --   pure (i, x + discountFactor*y)

-- -- environment :: (State -> Action -> IO (State, Double)) -> MonadicLens IO (State, Action) (State, Double) () ()
-- -- environment f = monadicLens @IO (const ()) (const . uncurry f)

-- learningRate :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double)
-- learningRate alpha = monadicLens @IO id $ \f (i, x) -> do {y <- f i; pure (i, alpha*x + (1 - alpha)*y)}

-- bellman :: Double -> MonadicLens IO (i -> IO Double) (i, Double) (i -> IO Double) (i, Double, i)
-- bellman discountFactor = monadicLens @IO id $ \f (i, x, j) -> do {y <- f j; pure (i, x + discountFactor*y)}

-- environment :: (state -> action -> IO (state, Double)) -> MonadicLens IO (state, action) (state, Double) () ()
-- environment f = monadicLens @IO (const ()) (const . uncurry f)

-- -- Compose Q-learning optic
-- qLearningPD :: MonadicLens IO (IOUArray (State, Action) Double) () (State, Action) ()
-- qLearningPD = update
--            . learningRate 0.2
--            . bellman 0.95
--            . environment (\s a -> pure (s, prisonersDilemmaMatrix a Testify))

-- -- Training loop
-- trainPD :: IOUArray (State, Action) Double -> Int -> IO ()
-- trainPD q n = replicateM_ n $ do
--   let state = 0
--       action = Testify -- could replace with ε-greedy sampling
--   qLearningPD (\_ -> pure ()) (state, action) q

-- -- Greedy strategy extraction
-- greedyPD :: IOUArray (State, Action) Double -> IO Action
-- greedyPD q = do
--   vS <- readArray q (0, StaySilent)
--   vT <- readArray q (0, Testify)
--   pure $ if vT >= vS then Testify else StaySilent

-- -- Run training and extract final strategy
-- learnPDStrategy :: IO Action
-- learnPDStrategy = do
--   q <- newArray qBounds 0.0
--   trainPD q 150
--   greedyPD q

-- testCompPD :: IO ()
-- testCompPD = do
--   putStrLn "Training Prisoner's Dilemma Q-learning..."
--   action <- learnPDStrategy
--   putStrLn $ "Greedy action after learning: " ++ show action
