{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Custom.HUBA_comp where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Custom.RLGeneral
import OpenGames.Custom.QLens_compositional
import OpenGames.Engine.BayesianGames (dependentDecision)
import Examples.Auctions.AuctionSupportFunctions
import Data.Map (Map, toList)
import qualified Data.Map as Map

import System.Random (randomRIO)

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral (qLearningLens, qLearningGreedyLens)

-- price = 1.01
-- actionSpace = [0.0, 0.25, 0.5, 0.75, 1.0]

-- computePayoffs :: [Double] -> [Double]
-- computePayoffs bids = bids 

-- computeReturns = [opengame|
--     inputs : bids ;
--     feedback:  ;
--     :-------------------:

--     inputs    : bids ;
--     feedback  :      ;
--     operation : forwardFunction computePayoffs ;
--     outputs   : payments ;
--     returns   :      ;

--     :-------------------:
--     outputs: payments ;
--     returns: ;
-- |]


-- huba actionSpace = [opengame|
--     inputs    :      ;
--     feedback  :      ;

--     :-----------------:

--     inputs    :      ;
--     feedback  :      ;
--     operation :  dependentDecision "Player1" (const actionSpace);
--     outputs   :  player1Bid ;
--     returns   :  payments  ;

--     inputs    :      ;
--     feedback  :      ;
--     operation :  dependentDecision "Player1" (const actionSpace);
--     outputs   :  player2Bid ;
--     returns   :  payments  ;

--     inputs    :      ;
--     feedback  :      ;
--     operation :  dependentDecision "Player1" (const actionSpace);
--     outputs   :  player3Bid ;
--     returns   :  payments  ;

--     inputs    :      ;
--     feedback  :      ;
--     operation :  dependentDecision "Player1" (const actionSpace);
--     outputs   :  player4Bid ;
--     returns   :  payments  ;

--     inputs    :  [player1Bid, player2Bid, player3Bid, player4Bid]  ;
--     feedback  :      ;
--     operation :   computeReturns  ;
--     outputs   :  payments ;
--     returns   :      ;
--     :-----------------:

--     outputs   :      ;
--     returns   :      ;
-- |]