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

module OpenGames.Custom.Games_with_outputs where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import OpenGames.Custom.PrisonersDilemma_new_comp
import OpenGames.Custom.BoS_comp

prisonersDilemmaInternalWithOutput = [opengame|
   inputs    :    ;
   feedback  :    ;
   :----------------------------:
   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player1" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :     ;
   feedback  :     ;
   operation : dependentDecision "player2" (const [StaySilent, Testify]) ;
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;
   :----------------------------:
   outputs   :  (decisionPlayer1, prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2)  ;
   returns   :    ;
|]

bachOrStravinskyWithOutputs = [opengame|
    inputs    :    ;
    feedback  :    ;
    :----------------------------:
    inputs    :    ;
    feedback  :    ;
    operation : dependentDecision "player1" (const [Bach, Stravinsky]) ;
    outputs   : decisionPlayer1 ;
    returns   : bosPayoffMatrix Player1 decisionPlayer1 decisionPlayer2 ;

    inputs    :     ;
    feedback  :     ;
    operation : dependentDecision "player2" (const [Bach, Stravinsky]) ;
    outputs   : decisionPlayer2 ;
    returns   : bosPayoffMatrix Player2 decisionPlayer1 decisionPlayer2 ;
    :----------------------------:
    outputs   :  (decisionPlayer1, bosPayoffMatrix Player1 decisionPlayer1 decisionPlayer2)  ;
    returns   :    ;
|]