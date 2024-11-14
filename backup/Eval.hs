{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Eval {-# DEPRECATED "No se ni porque" #-} where
import AST
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import Control.Monad
import Control.Monad.Except

hashtableSize = 50
success = "/"

type HashTable s k v = H.HashTable s k v
type Namespace s = HashTable s ProcId Proc
type Set s = HashTable s ProcId Bool

type EvalError = String

type Prog = [Sentence]

{- Cambiamos la representacion de los procesos usando las propiedades de
 - conmutatividad y asociatividad del operador de paralelismo para usar una
 - representacion de arbol binario completo
     ||                          ||
   a   ||           ===>     ||     ||
     b   ||                a   b  c   d
       c   d
-}

evalSent :: forall s. (Namespace s -> Sentence -> ST s ())
evalSent  defines (Assign id p) = H.insert defines id p

eval :: forall s. (Prog -> ST s (Namespace s))
eval prog = do
  defines <- H.newSized hashtableSize
  forM_ prog (evalSent defines)
  return defines

type Run = Event -> Proc
type RunConditionally = Event -> Either EvalError Proc
type Refusal = Event -> Bool
type RefusalConditionally = Event -> Either EvalError Bool

data EvalResult = EvalResult { run :: RunConditionally , refusal :: RefusalConditionally }

evalProc :: forall s. (Namespace s -> Proc -> ST s (EvalResult))
evalProc defines p = 
        case p of
          (ExternalChoice q r) -> wrapResult <$> (runExternalChoice defines q r)
                                             <*> (refusalExternalChoice defines q r)
          (Prefix pref q) -> return $ wrapResult (runPrefix pref q)
                                                 (refusalPrefix pref q)
          (Parallel q r) -> wrapResult <$> (runParallel defines q r)
                                       <*> (refusalParallel defines q r)
          (ByName q) -> do
                        definition <- H.lookup defines q
                        case definition of
                            Just q' -> evalProc defines q'
                            Nothing -> return $ wrapResult (undefinedError q) (undefinedError q)
          _ -> error ""

        where
          wrapResult run refusal = EvalResult { run = run, refusal = refusal }
          undefinedError q ev = throwError $ "Evaluation error: Current process has an undefined process ( " ++ q ++ " )\n"


data EvalStarResult s = EvalStarResult { runStar :: ([Event] -> ST s (Either EvalError Proc)) ,
                                         refusalStar :: ([Event] -> ST s (Either EvalError [Event])) }

evalProcStar :: Namespace s -> Proc -> (EvalStarResult s)
evalProcStar defines p = let
    runStar' :: [Event] -> ST s (Either EvalError Proc)
    runStar' evs = foldM evalRun (return p) evs
    refusalStar' :: [Event] -> ST s (Either EvalError [Event])
    refusalStar' evs = do
        q_E <- foldM evalRunRefuse (return (p, [])) evs
        return $ do
            (q, refusals) <- q_E
            return refusals
    in
        EvalStarResult { runStar = runStar' , refusalStar = refusalStar' }
    
    where
        evalRun :: Either EvalError Proc -> Event -> ST s (Either EvalError Proc)
        evalRun q ev = 
            case q of
                Left _ -> return q
                Right q' -> do
                    r <- evalProc defines q'
                    return (run r ev)
        evalRunRefuse :: Either EvalError (Proc, [Event]) -> Event -> ST s (Either EvalError (Proc, [Event]))
        evalRunRefuse q ev = do
            case q of
                Left _ -> return q
                Right (q', refusals) -> do
                    r <- evalProc defines q'
                    return $ do
                        run_r <- run r ev
                        refusal_r <- refusal r ev
                        if refusal_r
                        then return (run_r, ev:refusals)
                        else return (run_r, refusals)


{- Al usar RunConditionally y RefusalConditionally debemos checkear siempre su
 - retorno. Lo cual hace que debamos usar la monada Either recurrentemente
 -}

runParallel :: forall s. (Namespace s -> Proc -> Proc -> ST s (RunConditionally))
runParallel ns q r = do
         alph_q_E <- alpha ns q
         alph_r_E <- alpha ns r
         q' <- evalProc ns q
         r' <- evalProc ns r
         let
            returnPar :: Proc -> Proc -> Either EvalError Proc
            returnPar pa pb = case (pa, pb) of 
                                    (Stop, _) -> return pb
                                    (Skip, _) -> return pb
                                    (_, Stop) -> return pa
                                    (_, Skip) -> return pa
                                    (_, _) -> return (Parallel pa pb)
            runPar :: RunConditionally
            runPar ev = 
                do
                    alph_q <- alph_q_E
                    alph_r <- alph_r_E
                    refusal_q' <- if elem ev alph_q then refusal q' ev else return True
                    refusal_r' <- if elem ev alph_r then refusal r' ev else return True
                    if elem ev alph_q && elem ev alph_r
                    then do
                        run_q' <- if not refusal_q' && not refusal_r' then run q' ev else return q
                        run_r' <- if not refusal_q' && not refusal_r' then run r' ev else return r
                        returnPar run_q' run_r'
                    else do
                        run_q' <- if not refusal_q' then run q' ev else return q
                        run_r' <- if not refusal_r' then run r' ev else return r
                        -- throwError ((show refusal_q') ++ (show refusal_r') ++ (show run_q') ++ " ... " ++ (show run_r'))
                        returnPar run_q' run_r'

         return runPar

refusalParallel :: forall s. (Namespace s -> Proc -> Proc -> ST s (RefusalConditionally))
refusalParallel ns q r = do
         alph_q_E <- alpha ns q
         alph_r_E <- alpha ns r
         q' <- evalProc ns q
         r' <- evalProc ns r
         let
            refusalPar :: RefusalConditionally
            refusalPar ev = 
                do
                    alph_q <- alph_q_E
                    alph_r <- alph_r_E
                    refusal_q' <- if elem ev alph_q then refusal q' ev else return True
                    refusal_r' <- if elem ev alph_r then refusal r' ev else return True
                    if elem ev alph_q && elem ev alph_r
                    then if not refusal_q' && not refusal_r'
                        then return False
                        else return True
                    else if refusal_q'
                        then if refusal_r'
                            then return True
                            else return False
                        else return False

         return refusalPar

runPrefix :: Event -> Proc -> RunConditionally
runPrefix pref q = let
         runPref :: RunConditionally
         runPref ev = if ev == pref
                      then return q
                      else return $ Prefix pref q
         in
         runPref

refusalPrefix :: Event -> Proc -> RefusalConditionally
refusalPrefix pref q = let
         refusalPref :: RefusalConditionally
         refusalPref ev = if ev /= pref
                          then return True
                          else return False
         in
         refusalPref

runExternalChoice :: forall s. (Namespace s -> Proc -> Proc -> ST s (RunConditionally))
runExternalChoice defines q r = do
         q' <- evalProc defines q
         r' <- evalProc defines r
         let
            runExt :: RunConditionally
            runExt ev =
                do
                    refusal_q' <- refusal q' ev
                    if refusal_q'
                    then do
                            refusal_r' <- refusal r' ev
                            if refusal_r'
                            then return $ ExternalChoice q r
                            else run r' ev
                    else run q' ev

         return runExt

refusalExternalChoice :: forall s. (Namespace s -> Proc -> Proc -> ST s (RefusalConditionally))
refusalExternalChoice defines q r = do
         q' <- evalProc defines q
         r' <- evalProc defines r
         let
            refusalExt :: RefusalConditionally
            refusalExt ev = 
                do
                    refusal_q' <- refusal q' ev
                    if refusal_q'
                    then do
                            refusal_r' <- refusal r' ev
                            if refusal_r'
                            then return True
                            else return False
                    else return False

         return refusalExt


alpha :: forall s. (Namespace s -> Proc -> ST s (Either EvalError [Event]))
alpha ns p = do
        seen <- H.newSized hashtableSize
        alpha' ns seen p

alpha' :: forall s. (Namespace s -> Set s -> Proc -> (ST s (Either EvalError [Event])))
alpha' ns seen (InternalChoice p q) = do       --- caso interesante
        alph_p_E <- alpha' ns seen p
        alph_q_E <- alpha' ns seen q
        return $ (++) <$> alph_p_E <*> alph_q_E
{- Es posible evitar tanta verborragia de monadas de esta forma
alpha' ns seen (InternalChoice p q) = (liftA2 ++) <$> alpha' seen p <*> alpha' seen q
-}
alpha' ns seen (ExternalChoice p q) = do
        alph_p_E <- alpha' ns seen p
        alph_q_E <- alpha' ns seen q
        return $ (++) <$> alph_p_E <*> alph_q_E
alpha' ns seen (Parallel p q) = do
        alph_p_E <- alpha' ns seen p
        alph_q_E <- alpha' ns seen q
        return $ do
          alph_p <- alph_p_E
          alph_q <- alph_q_E
          return (alph_p ++ alph_q)
alpha' ns seen (Sequential p q) = do           --- caso interesante
        alph_p_E <- alpha' ns seen p
        alph_q_E <- alpha' ns seen q
        return $ do
          alph_p <- alph_p_E
          alph_q <- alph_q_E
          return (success:(alph_p ++ alph_q))
alpha' ns seen (Prefix pref q) = do            --- caso interesante
        events_E <- alpha' ns seen q
        return $ do
          events <- events_E
          return (pref:events)
alpha' ns seen (Interrupt p q) = do
        alph_p_E <- alpha' ns seen p
        alph_q_E <- alpha' ns seen q
        return $ do
          alph_p <- alph_p_E
          alph_q <- alph_q_E
          return (alph_p ++ alph_q)
alpha' ns seen (ByName p) =  do
        is_seen <- H.lookup seen p
        case is_seen of
          Just _ -> return $ return []
          Nothing -> do
              definition <- H.lookup ns p
              case definition of
                       Just q -> do
                          H.insert seen p True
                          alpha' ns seen q
                       Nothing -> return ((throwError $ "Evaluation error: Current process has an undefined process ( " ++ p ++" )\n")::Either EvalError [Event])
alpha' ns seen (Stop) = return $ return []
alpha' ns seen (Skip) = return $ return [success]
alpha' ns seen _ = return $ throwError "Evaluation error: Undefined"