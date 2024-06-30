module Utils (runProgWithEvents, runProgWithEvents') where

import AST
import Eval
import PrettyPrint
import Control.Monad.ST
import System.Random.Stateful (newSTGenM, mkStdGen)
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
runGenWithEvents :: Namespace s -> EvalRandom s -> Generic -> [Event] -> ST s Generic
runGenWithEvents defines erandom p evs = case p of
  (SentG (Assign _ q)) -> do
    let evalRes = evalProcStar defines erandom q
    q1 <- runStar evalRes evs
    return (ProcG q1)
  (SentG (Compare p q)) -> do
    let evalP = evalProcStar defines erandom p
    let evalQ = evalProcStar defines erandom q
    traceP <- trace evalP evs 
    traceQ <- trace evalQ evs
    if (traceP /= traceQ)
      then return (Error (show (prettyPrint (SentG (Compare p q)))))
      else do
        p1 <- runStar evalP evs
        q1 <- runStar evalQ evs
        return (SentG (Compare p1 q1))
  (ProcG p) -> do
    let evalRes = evalProcStar defines erandom p
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
  let progG = map SentG prog
  forM progG (flip (runGenWithEvents namespace erandom) evs)

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
  let progG = map SentG prog
  forM progG (flip (runGenWithEvents namespace erandom) evs)