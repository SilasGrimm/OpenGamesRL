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

module OpenGames.Custom.Evaluation_PD where

import OpenGames.Custom.PrisonersDilemma_new_comp
import qualified OpenGames.Custom.BoS_comp as BoS
import qualified OpenGames.Custom.Gift_exchange_game_comp as GEG
import qualified OpenGames.Custom.FPSBA_comp as FPSBA
import OpenGames.Engine.Engine

import Data.Map (Map, toList)
import qualified Data.Map as Map

import Data.List (groupBy, sortBy)
import Data.Function (on)


-- maps an opponent action for the prisoners dilemma to a tuple (meanQ, standardDeviationQ, isEquilibrium) for a specified amount of played games
evaluationPD :: Kleisli Stochastic () Action -> Int -> [([((State, Action), Double)], Bool)] -> IO ([Double], [Double], [Double], Double)
evaluationPD _ 0 acc = do
    -- putStrLn $ show acc

    let groupTuples :: Ord a => [(a, b)] -> [[(a, b)]]
        groupTuples = groupBy ((==) `on` fst) . sortBy (compare `on` fst)


    let stateActionVals = groupTuples $ concatMap fst acc
        sums = map (foldl (\((s, a), acc') v -> ((0, (snd . fst) v), snd v + acc')) ((0, Testify), 0.0)) stateActionVals
        means = map (\s -> (fst s, snd s / (fromIntegral (length acc)))) sums

        getMean :: (State, Action) -> Double
        getMean k = Map.findWithDefault 0 k (Map.fromList means)

        varianceSum = map (foldl (\acc' v -> ((snd v - getMean (fst v))**2 + acc')) 0.0) stateActionVals
        variance = map (\s -> s / (fromIntegral (length acc - 1))) varianceSum

        equilibriumProb = (foldl (\acc' (q, isEquilibrium) -> if isEquilibrium then acc' + 1 else acc') 0 acc) / fromIntegral (length acc)

    return (map snd means, variance, map sqrt variance, equilibriumProb)
    

evaluationPD opponentStrat n acc = do
    result <- evaluatePD opponentStrat
    evaluationPD opponentStrat (n - 1) (result : acc)

evaluatePD :: Kleisli Stochastic () Action -> IO ([((State, Action), Double)], Bool)
evaluatePD opponentStrat = do

    learnedQ <- learnPDStrategyComp initialQTable pdLens
    let strat = strategyFromLens learnedQ pdLensGreedy
        strategyTuple = strat ::- opponentStrat ::- Nil

        isEquilibrium = generateEquilibrium $ evaluate prisonersDilemmaInternal strategyTuple void

    return (Map.toList learnedQ, isEquilibrium)




evaluationBoS :: Kleisli Stochastic () BoS.ActionBoS -> Int -> [([((Int, BoS.ActionBoS), Double)], Bool)] -> IO ([Double], [Double], [Double], Double)
evaluationBoS _ 0 acc = do
    -- putStrLn $ show acc

    let groupTuples :: Ord a => [(a, b)] -> [[(a, b)]]
        groupTuples = groupBy ((==) `on` fst) . sortBy (compare `on` fst)


    let stateActionVals = groupTuples $ concatMap fst acc
        sums = map (foldl (\((s, a), acc') v -> ((0, (snd . fst) v), snd v + acc')) ((0, BoS.Bach), 0.0)) stateActionVals
        means = map (\s -> (fst s, snd s / (fromIntegral (length acc)))) sums

        getMean :: (Int, BoS.ActionBoS) -> Double
        getMean k = Map.findWithDefault 0 k (Map.fromList means)

        varianceSum = map (foldl (\acc' v -> ((snd v - getMean (fst v))**2 + acc')) 0.0) stateActionVals
        variance = map (\s -> s / (fromIntegral (length acc - 1))) varianceSum

        equilibriumProb = (foldl (\acc' (q, isEquilibrium) -> if isEquilibrium then acc' + 1 else acc') 0 acc) / fromIntegral (length acc)

    return (map snd means, variance, map sqrt variance, equilibriumProb)
    

evaluationBoS opponentStrat n acc = do
    result <- evaluateBoS opponentStrat
    evaluationBoS opponentStrat (n - 1) (result : acc)

evaluateBoS :: Kleisli Stochastic () BoS.ActionBoS -> IO ([((Int, BoS.ActionBoS), Double)], Bool)
evaluateBoS opponentStrat = do

    learnedQ <- BoS.learnBoSStrategy BoS.initialQTable BoS.bosLens opponentStrat
    let strat = BoS.strategyFromLens learnedQ BoS.bosGreedyLens
        strategyTuple = strat ::- opponentStrat ::- Nil

        isEquilibrium = generateEquilibrium $ evaluate BoS.bachOrStravinsky strategyTuple void

    return (Map.toList learnedQ, isEquilibrium)


evaluationGEG :: Kleisli Stochastic () GEG.EmployerAction -> Int -> [([((GEG.EmployerAction, GEG.EmployeeAction), Double)], Bool)] -> IO ([Double], [Double], [Double], Double)
evaluationGEG _ 0 acc = do
    -- putStrLn $ show acc

    let groupTuples :: Ord a => [(a, b)] -> [[(a, b)]]
        groupTuples = groupBy ((==) `on` fst) . sortBy (compare `on` fst)


    let stateActionVals = groupTuples $ concatMap fst acc
        sums = map (foldl (\((s, a), acc') v -> ((GEG.LowSalary, (snd . fst) v), snd v + acc')) ((GEG.LowSalary, GEG.LowEffort), 0.0)) stateActionVals
        means = map (\s -> (fst s, snd s / (fromIntegral (length acc)))) sums

        getMean :: (GEG.EmployerAction, GEG.EmployeeAction) -> Double
        getMean k = Map.findWithDefault 0 k (Map.fromList means)

        varianceSum = map (foldl (\acc' v -> ((snd v - getMean (fst v))**2 + acc')) 0.0) stateActionVals
        variance = map (\s -> s / (fromIntegral (length acc - 1))) varianceSum

        equilibriumProb = (foldl (\acc' (q, isEquilibrium) -> if isEquilibrium then acc' + 1 else acc') 0 acc) / fromIntegral (length acc)

    return (map snd means, variance, map sqrt variance, equilibriumProb)
    

evaluationGEG opponentStrat n acc = do
    result <- evaluateGEG opponentStrat
    evaluationGEG opponentStrat (n - 1) (result : acc)

evaluateGEG :: Kleisli Stochastic () GEG.EmployerAction -> IO ([((GEG.EmployerAction, GEG.EmployeeAction), Double)], Bool)
evaluateGEG opponentStrat = do

    learnedQ <- GEG.learnEmployeeGEGStrategy GEG.initialQTableEmployee GEG.gegLensEmployee
    let strat = GEG.employeeStrategyFromLens learnedQ GEG.gegGreedyLensEmployee
        strategyTuple = opponentStrat ::- strat ::- Nil

        isEquilibrium = generateEquilibrium $ evaluate GEG.giftExchangeGame strategyTuple void

    return (Map.toList learnedQ, isEquilibrium)




-- FPSBA takes too long to evaluate

evaluationFPSBA :: Kleisli Stochastic (FPSBA.PlayerName, FPSBA.PlayerValuation) FPSBA.Bid -> Int -> [([((FPSBA.PlayerValuation, FPSBA.Bid), Double)], Bool)] -> IO ([Double], [Double], [Double], Double)
evaluationFPSBA _ 0 acc = do
    -- putStrLn $ show acc

    let groupTuples :: Ord a => [(a, b)] -> [[(a, b)]]
        groupTuples = groupBy ((==) `on` fst) . sortBy (compare `on` fst)


    let stateActionVals = groupTuples $ concatMap fst acc
        sums = (Map.toList . Map.fromListWith (+) . concat) stateActionVals

    putStrLn $ show sums
        -- means = map (\s -> (fst s, snd s / (fromIntegral (length acc)))) sums

        -- getMean :: (GEG.EmployerAction, GEG.EmployeeAction) -> Double
        -- getMean k = Map.findWithDefault 0 k (Map.fromList means)

        -- varianceSum = map (foldl (\acc' v -> ((snd v - getMean (fst v))**2 + acc')) 0.0) stateActionVals
        -- variance = map (\s -> s / (fromIntegral (length acc - 1))) varianceSum

        -- equilibriumProb = (foldl (\acc' (q, isEquilibrium) -> if isEquilibrium then acc' + 1 else acc') 0 acc) / fromIntegral (length acc)

    -- return (map snd means, variance, map sqrt variance, equilibriumProb)
    return ([], [], [], 1.0)
    

evaluationFPSBA opponentStrat n acc = do
    result <- evaluateFPSBA opponentStrat
    evaluationFPSBA opponentStrat (n - 1) (result : acc)

evaluateFPSBA :: Kleisli Stochastic (FPSBA.PlayerName, FPSBA.PlayerValuation) FPSBA.Bid -> IO ([((FPSBA.PlayerValuation, FPSBA.Bid), Double)], Bool)
evaluateFPSBA opponentStrat = do

    learnedQ <- FPSBA.learnFPSBAStrategy FPSBA.initialQTable FPSBA.fpsbaLens
    let strat = FPSBA.strategyFromLens learnedQ FPSBA.fpsbaGreedyLens
        strategyTuple = strat ::- opponentStrat ::- Nil

        isEquilibrium = generateEquilibrium $ evaluate (FPSBA.firstPriceSealedBidAuction FPSBA.valueSpace FPSBA.actionSpace) strategyTuple void

    return (Map.toList learnedQ, isEquilibrium)
