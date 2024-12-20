module Utils (runProgWithEvents, runProgWithEvents') where

import AST
import Eval
import PrettyPrint
import Control.Monad.ST
import System.Random.Stateful (newSTGenM, mkStdGen, FrozenGen (freezeGen))
import Control.Monad (forM)

{- Copy-pasta from Interactive.hs
 - Arguments:
 -    defines: program symbols
 -    erandom: random number generator
 -    p: part of a program (using Generic)
 -    evs: events
 - Returns:
 -    st generic: result of run (error, or advanced version of p)
 -}
runGenWithEvents :: Namespace s -> EvalRandom -> Generic -> [Event] -> ST s Generic
runGenWithEvents defines erandom p evs = case p of
  (SentG (Assign _ q)) -> do
    let evalRes = evalProcStar defines erandom q
    q1 <- runStar evalRes evs
    return (ProcG q1)
  (SentG (NEq q r)) -> undefined
  (SentG (NEqStar q r)) -> undefined
  (SentG (Eq q r)) -> do
    let evalQ = evalProcStar defines erandom q
    let evalR = evalProcStar defines erandom r
    traceQ <- trace evalQ evs 
    traceR <- trace evalR evs
    if traceQ /= traceR
      then do
        {-traceShowM evs
        traceShowM traceQ
        traceShowM traceR
        traceShowM p-}
        return (Error (show (prettyPrint (SentG (Eq q r)))))
      else do
        p1 <- runStar evalQ evs
        q1 <- runStar evalR evs
        return (SentG (Eq p1 q1))
  (ProcG q) -> do
    let evalRes = evalProcStar defines erandom q
    q1 <- runStar evalRes evs
    return (ProcG q1)
  (Error err) -> return (Error err)

{- Single automated run. Useful for tests
 - Arguments:
 -    prog: program
 -    evs: events
 - Returns:
 -    st [generics]: state of the program after the events
 -}
runProgWithEvents :: Prog -> [Event] -> ST s [Generic]
runProgWithEvents prog evs = do
  namespace <- eval prog
  erandom <- newSTGenM (mkStdGen 2024)
  frozenRand <- freezeGen erandom
  let progG = map SentG prog
  forM progG (flip (runGenWithEvents namespace frozenRand) evs)

{- Single automated run. Useful for tests
 - Arguments:
 -    namespace: program symbols
 -    prog: program
 -    evs: events
 - Returns:
 -    st [generics]: state of the program after the events
 -}
runProgWithEvents' :: Namespace s -> Prog -> [Event] -> ST s [Generic]
runProgWithEvents' namespace prog evs = do
  erandom <- newSTGenM (mkStdGen 2024)
  frozenRand <- freezeGen erandom
  let progG = map SentG prog
  forM progG (flip (runGenWithEvents namespace frozenRand) evs)