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

-- |
-- Module      :  Eval
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with evaluation of abstract syntax trees of CSP.

module Eval (
  evalSent,
  eval,
  evalProc,
  evalProcStar,
  Namespace,
  EvalRandom,
  Prog,
  EvalResult(..),
  EvalStarResult(..),
  alpha
) where

import AST
    ( Sentence(Assign),
      Proc(Skip, InternalChoice, ExternalChoice, Parallel, Sequential,
           Prefix, Interrupt, ByName, Stop),
      Event,
      ProcId )
import Control.Monad ( foldM, forM_ )
import Control.Monad.ST ( ST )
import qualified Data.HashTable.ST.Basic as H
import System.Random.Stateful (STGenM, StdGen, applySTGen, TGen, STGen (unSTGen), FrozenGen (thawGen, freezeGen), RandomGen (genWord8))
import System.Random (random, RandomGen)

hashtableSize :: Int
hashtableSize = 50

success :: String
success = "/"

type HashTable s k v = H.HashTable s k v

type Namespace s = HashTable s ProcId Proc

{- STGenM no es apto para concurrencia -}
type EvalRandom = STGen StdGen

type Set s = HashTable s ProcId Bool

type Prog = [Sentence]

{- Cambiamos la representacion de los procesos usando las propiedades de
 - conmutatividad y asociatividad del operador de paralelismo para usar una
 - representacion de arbol binario completo
     ||                          ||
   a   ||           ===>     ||     ||
     b   ||                a   b  c   d
       c   d
-}

{- Evaluate a single sentence
 - Arguments:
 -    defines: record of the program symbols
 -    sentence: a sentence from the program
 - Returns:
 -    st: stateful computations
 -}
evalSent :: Namespace s -> Sentence -> ST s ()
evalSent defines (Assign id p) = H.insert defines id p
evalSent _ _ = return ()

{- Evaluate a single sentence
 - Arguments:
 -    prog: program
 - Returns:
 -    st namespace: stateful computation with the record of
 -      all program symbols
 -}
eval :: Prog -> ST s (Namespace s)
eval prog = do
  defines <- H.newSized hashtableSize
  forM_ prog (evalSent defines)
  return defines

type Run = Event -> Proc

type Refusal = Event -> Bool

data EvalResult = EvalResult {run :: Run, refusal :: Refusal}

data EvalStarResult s = EvalStarResult
  { runStar :: [Event] -> ST s (Proc),
    refusalStar :: [Event] -> ST s ([Event]),
    trace :: [Event] -> ST s ([Event])
  }

{- Evaluate a process and produce a run and a refuse function
 - Arguments:
 -    defines: record of the program symbols
 -    random: random number generator from system entropy or
 -      user-provided seed
 -    p: a process from the program
 - Returns:
 -    st evalresult: stateful computations with a run and a
 -      refuse function
 -}
evalProc :: Namespace s -> EvalRandom -> Proc -> ST s EvalResult
evalProc defines random p =
  case p of
    (ExternalChoice q r) ->
      wrapResult
        <$> runExternalChoice defines random q r
        <*> refusalExternalChoice defines random q r
    (Prefix pref q) ->
      return $
        wrapResult
          (runPrefix pref q)
          (refusalPrefix pref q)
    (Parallel q r) ->
      wrapResult
        <$> runParallel defines random q r
        <*> refusalParallel defines random q r
    (ByName q) -> do
      definition <- H.lookup defines q
      case definition of
        Just q' -> 
          wrapResult 
            <$> runByName defines random q q'
            <*> refusalByName defines random q q'
        Nothing -> error "Evaluation error: Some process has an undefined process ( " ++ q ++ " )\n" `seq` return ignore
    (InternalChoice q r) ->
      wrapResult
        <$> runInternalChoice defines random q r
        <*> refusalInternalChoice defines random q r
    (Sequential q r) -> do
      case q of
        Stop -> return $ wrapResult
            (const Stop)
            (const True)
        Skip -> evalProc defines random r
        _ -> do
          q' <- evalProc defines random q
          return $ wrapResult
              (\ev -> (Sequential (run q' ev) r))
              (refusal q')
    (Stop) -> return ignore
    (Skip) -> return ignore -- No contamos el evento interno "/"
    (Interrupt q r) ->
      wrapResult
        <$> runInterrupt defines random q r
        <*> refusalInterrupt defines random q r
    _ -> error ""
  where
    wrapResult run refusal = EvalResult {run = run, refusal = refusal}
    ignore = EvalResult { run = const p, refusal = const True }

{- Evaluate a process and produce a run and a refuse function
 -   that accept many events
 - Arguments:
 -    defines: record of the program symbols
 -    random: random number generator from system entropy or
 -      user-provided seed
 -    p: a process from the program
 - Returns:
 -    st evalresult: stateful computations with a run and a
 -      refuse function
 -}
evalProcStar :: Namespace s -> EvalRandom -> Proc -> EvalStarResult s
evalProcStar defines random p =
  let runStar' = foldM evalRun p
      refusalStar' evs = do
        (_, refusals) <- foldM evalRunRefuse (p, []) evs
        return refusals
      trace' evs = do
        (_, trace) <- foldM evalRunTrace (p, []) evs
        return trace
      evalRun q ev = do
            r <- evalProc defines random q
            return (run r ev)
      evalRunRefuse (q', refusals) ev = do
            r <- evalProc defines random q'
            let run' = run r ev
            let refusal' = refusal r ev
            if refusal'
              then return (run', ev : refusals)
              else return (run', refusals)
      evalRunTrace (q', trace) ev = do
            r <- evalProc defines random q'
            let run' = run r ev
            let refusal' = refusal r ev
            if refusal'
              then return (run', trace)
              else return (run', ev : trace)
   in EvalStarResult {runStar = runStar', refusalStar = refusalStar', trace = trace'}

{- Semantics of interrupt: try to run `r` or run `q`. If
 - `q` is run then `r` is still interrupting
 -}
runInterrupt :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Run
runInterrupt defines random q r = let
  runInter :: Namespace s -> EvalRandom -> ST s Run
  runInter defines random = do
    r' <- evalProc defines random r
    q' <- evalProc defines random q
    return (\ev ->
      if not (refusal r' ev)
        then run r' ev
        else (Interrupt (run q' ev) r))
  in
    runInter defines random

{- Semantics of interrupt: try to run `r` or run `q`. If 
 - `r` accepts then accept, else if `q` accepts accept
 -}
refusalInterrupt :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Refusal
refusalInterrupt defines random q r = let
  runInter :: Namespace s -> EvalRandom -> ST s Refusal
  runInter defines random = do
    r' <- evalProc defines random r
    q' <- evalProc defines random q
    return (\ev ->
      if not (refusal r' ev)
        then False
        else refusal q' ev)
  in
    runInter defines random

{- Semantics of internal choice: random between running `q`
 - or `r`
 -}
runInternalChoice :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Run
runInternalChoice ns erandom q r = let
  run_q :: Namespace s -> EvalRandom -> ST s Run -- No pueden usarse ns y erandom del scope :C
  run_q ns erandom = do
    q' <- evalProc ns erandom q
    return (\ev ->
      if refusal q' ev
        then (InternalChoice q r)
        else run q' ev)
  run_r :: Namespace s -> EvalRandom -> ST s Run
  run_r ns erandom = do
    r' <- evalProc ns erandom q
    return (\ev ->
      if refusal r' ev
        then (InternalChoice q r)
        else run r' ev)
  in do
    {-unfrozenGen <- thawGen erandom
    flip <- applySTGen (random :: RandomGen g => g -> (Int, g)) unfrozenGen
    -}
    let (flip, erandom') = genWord8 erandom
    if even flip
      then run_q ns erandom'
      else run_r ns erandom'

{- Random entre rechazar como `q` o `r`.
 - Puede haber un problema de coherencia si se paraleliza
 - el evaluador cada hilo recibi
 -}
refusalInternalChoice :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Refusal
refusalInternalChoice ns erandom q r = let
  refusal_q :: Namespace s -> EvalRandom -> ST s Refusal -- No pueden usarse ns y erandom del scope :C
  refusal_q ns erandom = do
    q' <- evalProc ns erandom q
    return (\ev ->
      if refusal q' ev
        then True
        else False)
  refusal_r :: Namespace s -> EvalRandom -> ST s Refusal
  refusal_r ns erandom = do
    r' <- evalProc ns erandom r
    return (\ev ->
      if refusal r' ev
        then True
        else False)
  in do
    let (flip, erandom') = genWord8 erandom
    if even flip
      then refusal_q ns erandom'
      else refusal_r ns erandom'

{- Ejecutar el proceso `q` -}
runByName :: Namespace s -> EvalRandom -> ProcId -> Proc -> ST s Run
runByName ns random name q = do
  q' <- evalProc ns random q
  return (\ev ->
      if refusal q' ev
        then (ByName name)
        else run q' ev)

{- Rechazar como el proceso `q` -}
refusalByName :: Namespace s -> EvalRandom -> ProcId -> Proc -> ST s Refusal
refusalByName ns random _ q = do
  q' <- evalProc ns random q
  return (refusal q')

runParallel :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Run
runParallel ns random q r = do
  alph_q <- alpha ns q
  alph_r <- alpha ns r
  q' <- evalProc ns random q
  r' <- evalProc ns random r
  let returnPar :: Proc -> Proc -> Proc
      returnPar pa pb = case (pa, pb) of
        (Stop, _) -> pb
        (Skip, _) -> pb
        (_, Stop) -> pa
        (_, Skip) -> pa
        (_, _) -> (Parallel pa pb)
      runPar :: Run
      runPar ev =
        do
          let refusal_q' = if ev `elem` alph_q then refusal q' ev else True
          let refusal_r' = if ev `elem` alph_r then refusal r' ev else True
          if ev `elem` alph_q && ev `elem` alph_r
            then do
              let run_q' = if not refusal_q' && not refusal_r' then run q' ev else q
              let run_r' = if not refusal_q' && not refusal_r' then run r' ev else r
              returnPar run_q' run_r'
            else do
              let run_q' = if not refusal_q' then run q' ev else q
              let run_r' = if not refusal_r' then run r' ev else r
              -- throwError ((show refusal_q') ++ (show refusal_r') ++ (show run_q') ++ " ... " ++ (show run_r'))
              returnPar run_q' run_r'

  return runPar

refusalParallel :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Refusal
refusalParallel ns random q r = do
  alph_q <- alpha ns q
  alph_r <- alpha ns r
  q' <- evalProc ns random q
  r' <- evalProc ns random r
  let refusalPar :: Refusal
      refusalPar ev =
        do
          let refusal_q' = if ev `elem` alph_q then refusal q' ev else True
          let refusal_r' = if ev `elem` alph_r then refusal r' ev else True
          if elem ev alph_q && elem ev alph_r
            then
              if not refusal_q' && not refusal_r'
                then False
                else True
            else
              (if refusal_q' && refusal_r' then True else False)

  return refusalPar

runPrefix :: Event -> Proc -> Run
runPrefix pref q =
  let runPref :: Run
      runPref ev =
        if ev == pref
          then q
          else Prefix pref q
   in runPref

refusalPrefix :: Event -> Proc -> Refusal
refusalPrefix pref _ =
  let refusalPref :: Refusal
      refusalPref ev =
        if ev /= pref
          then True
          else False
   in refusalPref

runExternalChoice :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Run
runExternalChoice defines random q r = do
  q' <- evalProc defines random q
  r' <- evalProc defines random r
  let runExt :: Run
      runExt ev =
        do
          let refusal_q' = refusal q' ev
          if refusal_q'
            then do
              let refusal_r' = refusal r' ev
              if refusal_r'
                then ExternalChoice q r
                else run r' ev
            else run q' ev

  return runExt

refusalExternalChoice :: Namespace s -> EvalRandom -> Proc -> Proc -> ST s Refusal
refusalExternalChoice defines random q r = do
  q' <- evalProc defines random q
  r' <- evalProc defines random r
  let refusalExt :: Refusal
      refusalExt ev =
        do
          let refusal_q' = refusal q' ev
          if refusal_q'
            then do
              let refusal_r' = refusal r' ev
              if refusal_r'
                then True
                else False
            else False

  return refusalExt

alpha :: Namespace s -> Proc -> ST s ([Event])
alpha ns p = do
  seen <- H.newSized hashtableSize
  alpha' ns seen p

alpha' :: Namespace s -> Set s -> Proc -> ST s ([Event])
alpha' ns seen (InternalChoice p q) = (++) <$> alpha' ns seen p <*> alpha' ns seen q
{- Es posible evitar tanta verborragia de monadas de esta forma
alpha' ns seen (InternalChoice p q) = (liftA2 ++) <$> alpha' seen p <*> alpha' seen q
-}
alpha' ns seen (ExternalChoice p q) = (++) <$> alpha' ns seen p <*> alpha' ns seen q
alpha' ns seen (Parallel p q) = (++) <$> alpha' ns seen p <*> alpha' ns seen q
alpha' ns seen (Interrupt p q) = (++) <$> alpha' ns seen p <*> alpha' ns seen q
alpha' ns seen (Sequential p q) = (++) <$> alpha' ns seen p <*> alpha' ns seen q
alpha' ns seen (Prefix pref q) = do
  --- caso interesante
  events <- alpha' ns seen q
  return (pref : events)
alpha' ns seen (ByName p) = do
  is_seen <- H.lookup seen p
  case is_seen of
    Just _ -> return []
    Nothing -> do
      definition <- H.lookup ns p
      case definition of
        Just q -> do
          H.insert seen p True
          alpha' ns seen q
        Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ p ++ " )\n" `seq` return []
alpha' _ _ Stop = return []
alpha' _ _ Skip = return [success]