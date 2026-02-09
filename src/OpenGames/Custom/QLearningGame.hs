{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module OpenGames.Custom.QLearningGame where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.BayesianGames (dependentDecision, fromLens)
-- import OpenGames.Engine.BayesianGames (dependentDecisionCustom)
import Examples.Auctions.AuctionSupportFunctions
import Data.Map (Map, toList)
import qualified Data.Map as Map
-- import           Data.HashMap                       as HM hiding (null,map,mapMaybe)

import System.Random (randomRIO)

import qualified Control.Applicative as Vector
import OpenGames.Custom.RLGeneral_new


type State = Int
data Action = Testify | StaySilent deriving (Eq, Ord, Show)

prisonersDilemmaMatrix :: Action -> Action -> Reward
prisonersDilemmaMatrix StaySilent StaySilent = -1
prisonersDilemmaMatrix StaySilent Testify = -3
prisonersDilemmaMatrix Testify StaySilent = 0
prisonersDilemmaMatrix Testify Testify = -2

concreteLens :: QLensNew (QTable State Action) State Action Reward
concreteLens = qLearningLens' 0.1 0.1 0.8 (const [StaySilent, Testify])

openGameFromLens = fromLens (deploy' concreteLens) (adapt' concreteLens)

qLearning = [opengame|
   inputs    :  (qTable, state)  ;
   feedback  :  qTable' ;
   :----------------------------:

   inputs    :  (qTable, state)  ;
   feedback  :  qTable'  ;
   operation :  openGameFromLens  ;
   outputs   :  actionDist ;
   returns   :  (s, a, r, s') ;

   :----------------------------:
   outputs   :  actionDist  ;
   returns   :  (s, a, r, s')  ;
|]

-- prisonersDilemmaInternal qTable state = [opengame|
--    inputs    :    ;
--    feedback  :    ;
--    :----------------------------:

--    inputs    : (qTable, state)   ;
--    feedback  : qTable'  ;
--    operation : qLearning ;
--    outputs   : action ;
--    returns   : (0, decisionPlayer1, prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2, Nothing) ;

--    inputs    :     ;
--    feedback  :     ;
--    operation : dependentDecision "player2" (const [StaySilent, Testify]) ;
--    outputs   : decisionPlayer2 ;
--    returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
--    :----------------------------:
--    outputs   :    ;
--    returns   :    ;
-- |]

-----------------------------------------------

composedLens :: QLensNew (QTable State Action) State Action Reward
composedLens = qLearningLens' 0.1 0.1 0.8 (const [StaySilent, Testify])

prisonersDilemmaInternal = [opengame|
   inputs    :    ;
   feedback  :    ;
   :----------------------------:

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player1" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player2" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player1" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer12 ;
   returns   : prisonersDilemmaMatrix decisionPlayer12 decisionPlayer2 ;
   :----------------------------:
   outputs   :    ;
   returns   :    ;
|]

initialQTable = Map.fromList [((0, StaySilent), 0), ((0, Testify), 0)]

strategyFromLens :: QTable State Action -> QLensNew (QTable State Action) State Action Reward -> Kleisli Stochastic () Action
strategyFromLens q lens = Kleisli $ \() ->
  distFromList $ deploy' lens (q, 0)

alwaysTestify = Kleisli $ \() ->
  distFromList [(StaySilent, 0), (Testify, 1)]

isEquilibriumPrisonersDilemmaCustom strategyTuple = generateIsEq $ evaluate prisonersDilemmaInternal strategyTuple void