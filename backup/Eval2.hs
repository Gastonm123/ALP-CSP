{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      :  Eval
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with evaluation of abstract syntax trees of CSP.

module Eval1 (
  evalSent,
  eval,
  evalProc,
  Namespace,
  Trigger,
  Alpha,
  CacheTrigger,
  CacheAlpha,
  Automata
) where

import AST
    ( Sentence(Assign),
      Proc(Skip, InternalChoice, ExternalChoice, Parallel, Sequential,
           Prefix, Interrupt, ByName, Stop),
      Event )
-- import System.Random.Stateful (STGenM, StdGen)
import Control.Monad.State (State, MonadState (get, put), modify, runState, modify')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (forM)
import Data.Maybe (fromJust, fromMaybe)
import Data.List ((!?))
-- import System.Random (RandomGen, StdGen, Random (random))

{- STGenM no es apto para concurrencia -}
-- type EvalRandom s = STGenM StdGen s

type Prog = [Sentence]

{- Cambiamos la representacion de los procesos usando las propiedades de
 - conmutatividad y asociatividad del operador de paralelismo para usar una
 - representacion de arbol binario completo
     ||                          ||
   a   ||           ===>     ||     ||
     b   ||                a   b  c   d
       c   d
  ^                        ^
  cuadratico por las       nlogn por las
  llamadas a alpha         llamadas a alpha
-}

{- Evaluate a single sentence
 - Arguments:
 -    defines: record of the program symbols
 -    sentence: a sentence from the program
 - Returns:
 -    st: stateful computations
 -}
evalSent :: Sentence -> State Namespace ()
evalSent (Assign id p) = modify (Map.insert id p)
evalSent _ = return ()

{- Evaluate a program and return the definitions
 - Arguments:
 -    prog: program
 - Returns:
 -    st namespace: stateful computation with the record of
 -      all program symbols
 -}
eval :: Prog -> Namespace
eval prog = let
    (_, defines) = (runState (forM prog evalSent) Map.empty)
  in
    defines


data Automata = AParallel Proc Proc Trigger Trigger Trigger
              | AInterrupt Proc Proc Trigger Trigger
              | AInternal Proc Proc Trigger Trigger
              | AExternal Proc Proc Trigger Trigger
              | ASequential Proc Proc Trigger
              | APrefix Proc Trigger
              | AStop
              | ASkip

-- type Bindings = Map String GenericVal
type Namespace = Map.Map String Proc
type Seen = Set.Set String
type CacheAlpha = Map.Map String Alpha
type CacheTrigger = Map.Map String Trigger
type Trigger = [Set.Set Event]
type Alpha = [Event]

{-
a=
runAutomata :: Automata -> Event -> State (Namespace, CacheAlpha, CacheTrigger, StdGen) Automata
runAutomata a ev = do
  (namespace, cacheAlpha, cacheTrigger, randomGen) <- get
  let (coin, randomGen') = random randomGen :: (Int, StdGen)
  let coin' = coin `mod` 2
  let (automata, (_, cacheAlpha', cacheTrigger')) = runAutomata' a ev coin
  put (namespace, cacheAlpha, cacheTrigger, randomGen')

runAutomata' :: Automata -> Event -> Int -> State (Namespace, CacheAlpha, CacheTrigger) Automata
runAutomata' a ev coin = do
  case a of
    (AParallel p q triggerParallel triggerP triggerQ) -> let
        triggerChoosenParallel = fromMaybe 
            (head triggerParallel) 
            (triggerParallel !? coin)
        (triggerChoosenP, triggerChoosenQ) = chooseTrigger triggerP triggerQ coin
        p' = advance p ev -- lazy
        q' = advance q ev
      in
        if
          | not (ev `Set.elem` triggerChoosenParallel) -> return a
          | ev `Set.elem` triggerChoosenP &&
            ev `Set.elem` triggerChoosenQ -> AParallel p' q' <$> trigger (Parallel p' q') <*> trigger p' <*> trigger q'
          | ev `Set.elem` triggerChoosenP -> AParallel p' q  <$> trigger (Parallel p' q) <*> trigger p' <*> triggerQ
          | ev `Set.elem` triggerChoosenQ -> AParallel p  q' <$> trigger (Parallel p' q) <*> triggerP <*> trigger q'
    (AInterrupt p q triggerP triggerQ) -> let
        (triggerChoosenP, triggerChoosenQ) = chooseTrigger triggerP triggerQ coin
      in
        if
          | ev `Set.elem` triggerChoosenQ -> evalProc q'
          | ev `Set.elem` triggerChoosenP -> AInterrupt p' q <$> trigger p' <*> triggerQ
          | otherwise -> return a 
    (AExternal p q triggerP triggerQ) -> let
        (triggerChoosenP, triggerChoosenQ) = chooseTrigger triggerP triggerQ coin
      in
        if
          | ev `Set.elem` triggerChoosenP -> evalProc p'
          | ev `Set.elem` triggerChoosenQ -> evalProc q'
          | otherwise -> return a 
    (AInternal p q triggerP triggerQ) -> let
        (triggerChoosenP, triggerChoosenQ) = chooseTrigger triggerP triggerQ coin
      in
        if
          | even coin && ev `Set.elem` triggerChoosenP -> evalProc p'
          | odd coin  && ev `Set.elem` triggerChoosenQ -> evalProc q'
          | otherwise -> return a

chooseTrigger triggerP triggerQ coin = (
  fromMaybe
      (head triggerP)
      (triggerP !? coin),
  fromMaybe
      (head triggerQ)
      (triggerQ !? coin))
-}

evalProc :: Proc -> State (Namespace, CacheAlpha, CacheTrigger) Automata
evalProc p =
  case p of
    (Parallel q r) -> AParallel q r <$> (trigger (Parallel q r)) <*> (trigger q) <*> (trigger r)
    (Interrupt q r) -> AInterrupt q r <$> (trigger q) <*> (trigger r)
    (ExternalChoice q r) -> AExternal q r <$> (trigger q) <*> (trigger r)
    (InternalChoice q r) -> AInternal q r <$> (trigger q) <*> (trigger r)
    (Sequential q r) | q == Stop -> return AStop
                     | q == Skip -> evalProc r
                     | otherwise -> ASequential q r <$> (trigger q)
    (Prefix pref q) -> return (APrefix q [Set.singleton pref])
    (ByName name) -> do
      (namespace, _, _) <- get
      evalProc (fromJust (Map.lookup name namespace))
    (Stop) -> return AStop
    (Skip) -> return ASkip


trigger :: Proc -> State (Namespace, CacheAlpha, CacheTrigger) Trigger
trigger p = do
  (namespace, cacheAlpha, cacheTrigger) <- get
  let seen = Set.empty
  let {
    (a, (_, cacheAlpha', cacheTrigger', _)) = 
      runState 
        (trigger' p) 
        (namespace, cacheAlpha, cacheTrigger, seen)
  }
  put (namespace, cacheAlpha', cacheTrigger')
  return a


trigger'  :: Proc -> State (Namespace, CacheAlpha, CacheTrigger, Seen) Trigger
trigger' (Prefix pref _) = return [Set.singleton pref]
trigger' (ExternalChoice p q) = (nonDeterminism Set.union) <$> (trigger' p) <*> (trigger' q)
trigger' (InternalChoice p q) = do
  triggerP <- trigger' p
  triggerQ <- trigger' q
  {- la lista representa una tirada de moneda: un 0 hace que el trigger
   - de la seleccion sea el "trigger 0" de p, "trigger 0" debido a que
   - p puede ser no determinista por su parte, y un 1 hace que el
   - trigger sea el "trigger 1" de q, o en su defecto el "trigger 0"
   -}
  return [head triggerP, fromMaybe (head triggerQ) (triggerQ !? 1)]
trigger' (Parallel p q) = do
  triggerP <- trigger' p
  triggerQ <- trigger' q
  (namespace, cacheAlpha, cacheTrigger, seen) <- get
  let
    (alphL, (_, cacheAlpha1)) = runState (alpha p) (namespace, cacheAlpha)
    (alphR, (_, cacheAlpha2)) = runState (alpha q) (namespace, cacheAlpha1)
    alphL' = Set.fromList alphL
    alphR' = Set.fromList alphR
    triggerParallel trigL trigR =
      ((trigL `Set.union` trigR) `Set.difference` (alphL' `Set.intersection` alphR'))
      `Set.union` (trigL `Set.intersection` trigR)
  modify' (const (namespace, cacheAlpha2, cacheTrigger, seen))
  return (nonDeterminism triggerParallel triggerP triggerQ)
trigger' (Interrupt p q) = (nonDeterminism Set.union) <$> (trigger' p) <*> (trigger' q)
trigger' (ByName name) = do
  (namespace, cacheAlpha, cacheTrigger, seen) <- get :: (State 
        (Namespace, CacheAlpha, CacheTrigger, Seen)
        (Namespace, CacheAlpha, CacheTrigger, Seen))
  if
    | Map.member name cacheTrigger -> return (fromJust (Map.lookup name cacheTrigger))
    | Set.member name seen -> return [Set.empty]
    | otherwise -> do
        modify' (const (namespace, cacheAlpha, cacheTrigger, Set.insert name seen))
        let p    = (fromJust (Map.lookup name namespace))
        triggerP <- trigger' p
        modify' (\
          (_, cacheAlpha', cacheTrigger', seen') -> 
          (namespace, cacheAlpha', Map.insert name triggerP cacheTrigger', seen'))
        return triggerP
trigger' (Sequential p q) | p == Stop = return [Set.empty]
                         | p == Skip = trigger' q
                         | otherwise = trigger' p
trigger' Stop = return [Set.empty]
trigger' Skip = return [Set.empty] -- ignoramos el evento "success" interno

-- nonDeterminism :: (Set.Set -> Set.Set -> Set.Set) -> [Set.Set] -> [Set.Set] -> [Set.Set]
nonDeterminism :: (Set.Set a -> Set.Set a -> Set.Set a) -> 
                  [Set.Set a] -> 
                  [Set.Set a] ->
                  [Set.Set a]
nonDeterminism combFunc xss yss = let
    len = max (length xss) (length yss)
  in
    [combFunc
        (fromMaybe Set.empty (xss !? i))
        (fromMaybe Set.empty (yss !? i)) | i <- [0..len]]


alpha :: Proc -> State (Namespace, CacheAlpha) Alpha
alpha p = do
  (namespace, cache) <- get
  let seen = Set.empty
  let (a, (_, cache', _)) = (runState (alpha' p) (namespace, cache, seen))
  put (namespace, cache')
  return a

alpha' :: Proc -> State (Namespace, CacheAlpha, Seen) Alpha
alpha' (ByName name) = do
  (namespace, cache, seen) <- get :: (State 
      (Namespace, CacheAlpha, Seen)
      (Namespace, CacheAlpha, Seen))
  if
    | Map.member name cache -> return (fromJust (Map.lookup name cache))
    | Set.member name seen -> return []
    | otherwise -> do
        modify' (const (namespace, cache, Set.insert name seen))
        let p = (fromJust (Map.lookup name namespace))
        alphP <- alpha' p
        modify' (\
          (_, cache', seen') -> 
          (namespace, Map.insert name alphP cache', seen'))
        return alphP
alpha' (InternalChoice p q) = (++) <$> alpha' p <*> alpha' q
alpha' (ExternalChoice p q) = (++) <$> alpha' p <*> alpha' q
alpha' (Parallel p q) = (++) <$> alpha' p <*> alpha' q
alpha' (Sequential p q) = (++) <$> alpha' p <*> alpha' q
alpha' (Prefix pref q) = do
  events <- alpha' q
  return (pref : events)
alpha' (Interrupt p q) = (++) <$> alpha' p <*> alpha' q
alpha' Stop = return []
alpha' Skip = return [] -- ignoramos el evento "success" interno