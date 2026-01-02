-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE FlexibleContexts #-}

-- module OpenGames.Custom.PrisonersDilemmaRepeated where

-- import OpenGames.Engine.Engine
-- import OpenGames.Preprocessor
-- import OpenGames.Custom.RLLens (Action, Reward, QTable, State)
-- import OpenGames.Custom.ModifiedRLLens
-- import OpenGames.Custom.PrisonersDilemmaExternal (prisonersDilemmaMatrix, sample)

-- import           Control.Monad.State  hiding (lift, state,void, State)
-- import qualified Control.Monad.State  as ST

-- import Data.Map (Map)
-- import qualified Data.Map as Map

-- import Numeric.Probability.Distribution hiding (map, lift, filter)

-- prisonersDilemmaInternal = [opengame|
--    inputs    : (prevDec1, prevDec2) ;
--    feedback  :      ;
--    :----------------------------:
--    inputs    :  (prevDec1, prevDec2)   ;
--    feedback  :    ;
--    operation : dependentDecision "player1" (const [0, 1]) ;
--    outputs   : decisionPlayer1 ;
--    returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

--    inputs    : (prevDec1, prevDec2)    ;
--    feedback  :    ;
--    operation : dependentDecision "player2" (const [0, 1]) ;
--    outputs   : decisionPlayer2 ;
--    returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
--    :----------------------------:
--    outputs   : (decisionPlayer1, decisionPlayer2)   ;
--    returns   :     ;
-- |]

-- initialQTable = Map.fromList [((0, 0), 0), ((0, 1), 0)]

-- strategyFromLens :: QTable -> QLens QTable State Action Reward -> Kleisli Stochastic (Action, Action) Action
-- strategyFromLens qTable lens = Kleisli $ \(a1, a2) ->
--   distFromList $ deploy lens qTable 0

-- strategy1 :: Kleisli Stochastic (Action, Action) Action
-- strategy1 = strategyFromLens initialQTable qLearningLensNew

-- strategy2 :: Kleisli Stochastic (Action, Action) Action
-- strategy2 = strategyFromLens initialQTable qLearningLensNew

-- alwaysDefect = Kleisli $ \(a1, a2) ->
--   distFromList $ [(0, 0), (1, 1)]

-- stratTupleQ = strategy1 ::- strategy1 ::- Nil
-- stratTupleDefect = strategy1 ::- alwaysDefect ::- Nil

-- -- extract continuation
-- extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
-- extractContinuation (StochasticStatefulOptic v u) x = do
--   (z,a) <- ST.lift (v x)
--   u z ()

-- -- extract next state (action)
-- extractNextState :: StochasticStatefulOptic s () a () -> s -> Stochastic a
-- extractNextState (StochasticStatefulOptic v _) x = do
--   (z,a) <- v x
--   pure a



-- -- determine continuation for iterator, with the same repeated strategy
-- determineContinuationPayoffs :: Integer
--                              -> List
--                                       '[Kleisli Stochastic (Action, Action) Action,
--                                         Kleisli Stochastic (Action, Action) Action]
--                              -> (Action,Action)
--                              -> StateT Vector Stochastic ()
-- determineContinuationPayoffs 1        strat action = pure ()
-- determineContinuationPayoffs iterator strat action = do
--    extractContinuation executeStrat action
--    nextInput <- ST.lift $ extractNextState executeStrat action
--    determineContinuationPayoffs (pred iterator) strat nextInput
--  where executeStrat =  play prisonersDilemmaInternal strat


-- -- fix context used for the evaluation
-- contextCont iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action)



-- repeatedPDEq iterator strat initialAction = evaluate prisonersDilemmaInternal strat context
--   where context  = contextCont iterator strat initialAction





-- eqOutput iterator strat initialAction = generateIsEq $ repeatedPDEq iterator strat initialAction


-- -------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module OpenGames.Custom.PrisonersDilemmaRepeated where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral (Reward, QTable, Sample, qlens, QLens, computeTarget, deploy, adapt)
import OpenGames.Custom.PrisonersDilemmaExternal (prisonersDilemmaMatrix, sample)

import           Control.Monad.State  hiding (lift, state,void, State)
import qualified Control.Monad.State  as ST

import Data.Map (Map)
import qualified Data.Map as Map

import Numeric.Probability.Distribution hiding (map, lift, filter)
import OpenGames.Custom.RLLens (Action)

type RepeatedPDState = (Int, Int) -- the previous action of both inmates
type RepeatedPDAction = Int


qLearningLensRepeatedPD = qlens (const [0, 1])


prisonersDilemmaInternal = [opengame|
   inputs    : (prevDec1, prevDec2) ;
   feedback  :      ;
   :----------------------------:
   inputs    :  (prevDec1, prevDec2)   ;
   feedback  :    ;
   operation : dependentDecision "player1" (const [0, 1]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    : (prevDec1, prevDec2)    ;
   feedback  :    ;
   operation : dependentDecision "player2" (const [0, 1]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   : (decisionPlayer1, decisionPlayer2)   ;
   returns   :     ;
|]
-- compare to python implementation



initialQTable = Map.fromList [(((0, 0), 0), 0), (((0, 1), 0), 0), (((1, 0), 0), 0), (((1, 1), 0), 0),
                              (((0, 0), 1), 0), (((0, 1), 1), 0), (((1, 0), 1), 0), (((1, 1), 1), 0)] -- all possible scenarios initialized with 0 reward

strategyFromLens :: QTable RepeatedPDState RepeatedPDAction -> QLens (QTable RepeatedPDState RepeatedPDAction) RepeatedPDState RepeatedPDAction Reward -> Kleisli Stochastic RepeatedPDState RepeatedPDAction
strategyFromLens qTable lens = Kleisli $ \(a1, a2) -> distFromList $ deploy lens qTable (a1, a2)
-- qTable update inside the game itself?
    -- lensStrategy :: QTable RepeatedPDState RepeatedPDAction -> QLens (QTable RepeatedPDState RepeatedPDAction) RepeatedPDState RepeatedPDAction Reward 
    --                 -> RepeatedPDState -> T Double RepeatedPDAction
    -- lensStrategy q l (a1, a2) =
    --   let actionDist = distFromList $ deploy l q (a1, a2)

strategy1 :: Kleisli Stochastic RepeatedPDState RepeatedPDAction
strategy1 = strategyFromLens initialQTable qLearningLensRepeatedPD

alwaysDefect = Kleisli $ \(a1, a2) ->
  distFromList $ [(0, 0), (1, 1)]

stratTupleQ = strategy1 ::- strategy1 ::- Nil
stratTupleDefect = strategy1 ::- alwaysDefect ::- Nil

-- extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: StochasticStatefulOptic s () a () -> s -> Stochastic a
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a



-- determine continuation for iterator, with the same repeated strategy
-- takes number of rounds to play, qTables, the strategies of the players and current state (last chosen actions)
determineContinuationPayoffs :: Integer
                             -> (QTable RepeatedPDState RepeatedPDAction, QTable RepeatedPDState RepeatedPDAction) 
                             -> List
                                      '[Kleisli Stochastic RepeatedPDState RepeatedPDAction,
                                        Kleisli Stochastic RepeatedPDState RepeatedPDAction]
                             -> (RepeatedPDAction,RepeatedPDAction)
                             -> StateT Vector Stochastic ()
determineContinuationPayoffs 1   (q1, q2)    strat action = pure ()
determineContinuationPayoffs iterator (q1, q2) strat action = do
  extractContinuation executeStrat action -- execute strategies
  nextInput <- ST.lift $ extractNextState executeStrat action -- the next action pair chosen based on the strategies

   -- change strategy, so that we use the updated qTable
  let newQ1 = adapt qLearningLensRepeatedPD q1 (action, fst nextInput, prisonersDilemmaMatrix (fst action) (snd action), nextInput)
      newQ2 = adapt qLearningLensRepeatedPD q2 (action, snd nextInput, prisonersDilemmaMatrix (fst action) (snd action), nextInput)
      newStrat = strategyFromLens newQ1 qLearningLensRepeatedPD ::- strategyFromLens newQ2 qLearningLensRepeatedPD ::- Nil
   -----------

  determineContinuationPayoffs (pred iterator) (newQ1, newQ2) newStrat nextInput
 where executeStrat =  play prisonersDilemmaInternal strat


-- fix context used for the evaluation
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator (initialQTable, initialQTable) strat action)



repeatedPDEq iterator strat initialAction = evaluate prisonersDilemmaInternal strat context
  where context  = contextCont iterator strat initialAction





eqOutput iterator strat initialAction = generateIsEq $ repeatedPDEq iterator strat initialAction

