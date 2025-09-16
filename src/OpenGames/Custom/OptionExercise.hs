{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module OpenGames.Custom.OptionExercise where

import Data.Map (Map)
import qualified Data.Map as Map
import OpenGames.Custom.RLLens (Action)

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLLens hiding (deploy, adapt)
import OpenGames.Custom.ModifiedRLLens
import OpenGames.Custom.PrisonersDilemmaExternal (prisonersDilemmaMatrix, sample)

import           Control.Monad.State  hiding (lift, state,void, State)
import qualified Control.Monad.State  as ST

import Numeric.Probability.Distribution hiding (map, lift, filter)

-- Option Stopping Problem for american call options

data OptionAction = Exercise | Hold deriving (Show)

instance Ord OptionAction where
    (<=) Exercise Hold = False
    (<=) Hold Exercise = True 
    (<=) _ _ = False

instance Eq OptionAction where
    (==) Exercise Hold = False
    (==) Hold Exercise = False
    (==) _ _ = True

type OptionPrice = Int
type StrikePrice = Int
type TimeToMaturity = Int
type OptionState = (OptionPrice, StrikePrice, TimeToMaturity)
type OptionReward = Double

type OptionQTable = Map (OptionPrice, StrikePrice, TimeToMaturity, OptionAction) Double

-- qLearningLensOption :: QLens OptionQTable OptionState OptionAction OptionReward
-- qLearningLensOption = QLens 
--   {
--     deploy = \qTable -> (\(sPrice, sTime) -> createProbabilitiesFromRewards [(a, r) | ((sPrice', sTime', a), r) <- Map.toList qTable, (sPrice', sTime') == (sPrice, sTime)]) -- gets a qTable as argument and returns a function from a state to a distribution of actions for that state
--     , 
--     adapt = \q sample@(s, a, r, s') ->
--                 let target = computeTarget q gamma sample
--                 in qUpdate alpha q ((s, a), target)
--   }


-- reward :: OptionAction -> Double -> Double -> Double
-- reward Exercise strike spot = max profit 0
--                 where profit = strike - spot
-- reward Hold strike spot = 0

-- optionQ :: OptionQTable
-- optionQ = Map.fromList[((x, y, a), 0) | x <- [10..30], y <- [1..20], a <- [Exercise, Hold]] -- initialize reward with 0

-- optionStrategy :: Kleisli Stochastic OptionState OptionAction
-- optionStrategy = 
--     Kleisli $ \(price, maturity) -> distFromList $ deploy qLearningLensOption optionQ (price, maturity) 

