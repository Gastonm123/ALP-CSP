{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Elab where

import Lang
import Control.Monad.State
import Control.Monad (when)

elabProg :: SProg -> Prog
elabProg (SProg sents trace) = Prog (map elabSent sents) trace

elabSent :: SSentence -> Sentence
elabSent (SAssign pRef p) = let
    pars = sparams pRef
    name = sprocName pRef
    (normParams, normP) = runState (mapM elabParam pars) p
    in (Assign (ProcRef name normParams) (directElabProc normP))

type IndexName = String

elabParam :: SParameter -> State SProc Parameter
elabParam par = do
    p' <- get
    case par of
        (SOp m "+" c) -> do
            let maxDiff = maxIdxDiff m 0 p'
            when (maxDiff > 0) (put (normalizeIdxPar m maxDiff p'))
            return (Inductive m (c+maxDiff))
        (SOp m "-" c) -> do
            let maxDiff = maxIdxDiff m c p'
            when (maxDiff > 0) (put (normalizeIdxPar m maxDiff p'))
            return (Inductive m (maxDiff-c))
        (SBase n) -> return (Base n)
        _ -> error ("Parametro con operador invalido: " ++ show par)

-- calcula el mayor natural que se resta a cualquier indice o parametro
-- llamado `n`
maxIdxDiff :: IndexName -> Int -> SProc -> Int
maxIdxDiff n c sp = foldSProc maxIdxParam c sp
    where
        maxIdxParam (Left (SIVar _)) acc = acc
        maxIdxParam (Left (SIVal _)) acc = acc
        maxIdxParam (Left (SIOp _ "+" _)) acc = acc
        maxIdxParam (Left (SIOp m "-" c)) acc = if n == m then max c acc else acc
        maxIdxParam (Right (SBase _)) acc = acc
        maxIdxParam (Right (SOp _ "+" _)) acc = acc
        maxIdxParam (Right (SOp m "-" c)) acc = if n == m then max c acc else acc


normalizeIdxPar :: IndexName -> Int -> SProc -> SProc
normalizeIdxPar n diff = go
    where
        go (SInternalChoice       p q) = (SInternalChoice (go p) (go q))
        go (SExternalChoice       p q) = (SExternalChoice (go p) (go q))
        go (SLabeledAlt           p q) = (SLabeledAlt     (go p) (go q))
        go (SParallel             p q) = (SParallel       (go p) (go q))
        go (SSequential           p q) = (SSequential     (go p) (go q))
        go (SPrefix    (SEvent m i) q) = (SPrefix (SEvent m (map normIdx i)) (go q))
        go (SByName (SProcRef m pars)) = (SByName (SProcRef m (map normPar pars)))
        go b = b
        normIdx (SIVal      v) = (SIVal v)
        normIdx (SIVar      m) = if m == n then (SIOp m "+" diff) else (SIVar m)
        normIdx (SIOp m "+" c) = if m == n
            then (SIOp m "+" (c+diff)) else (SIOp m "+" c)
        normIdx (SIOp m "-" c) = if m == n
            then if diff-c>0 then (SIOp m "+" (diff-c)) else (SIVar m)
            else (SIOp m "+" c)
        normPar (SBase m) = (SBase m)
        normPar (SOp m "+" c) = if m == n
            then (SOp m "+" (c+diff)) else (SOp m "+" c)
        normPar (SOp m "-" c) = if m == n
            then (SOp m "+" (diff-c))
            else (SOp m "-" c)

-- `foldSProc f z proc` es identico a `foldr f z (inOrder proc)`
-- inOrder ignora los operadores y solo devuelve los eventos y
-- procesos (como eithers)
foldSProc :: (Either SIndex SParameter -> b -> b) -> b -> SProc -> b
foldSProc f z (SInternalChoice       p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SExternalChoice       p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SLabeledAlt           p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SParallel             p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SSequential           p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SInterrupt            _ q) = foldSProc f (foldSProc f z q) q
foldSProc f z (SPrefix   (SEvent _ is) q) = foldr (f . Left) (foldSProc f z q) is
foldSProc f z (SByName (SProcRef _ pars)) = foldr (f . Right) z pars
foldSProc _ z SStop = z
foldSProc _ z SSkip = z

directElabProc :: SProc -> Proc
directElabProc = go
    where
        go (SInternalChoice       p q) = InternalChoice (go p) (go q)
        go (SExternalChoice       p q) = ExternalChoice (go p) (go q)
        go (SLabeledAlt           p q) = LabeledAlt     (go p) (go q)
        go (SParallel             p q) = Parallel       (go p) (go q)
        go (SSequential           p q) = Sequential     (go p) (go q)
        go (SInterrupt            p q) = Interrupt      (go p) (go q)
        go (SPrefix   (SEvent n is) p) = Prefix (Event n (map directElabIdx is)) (go p)
        go (SByName (SProcRef n pars)) = ByName (ProcRef n (map directElabPar pars))
        go                       SStop = Stop
        go                       SSkip = Skip
        directElabPar p = case p of
            (SOp m "+" c) -> Inductive m c
            (SBase m) -> Base m
            _ -> error ("Parametro invalido: " ++ show p)
        directElabIdx i = case i of
            (SIOp m "+" c) -> IPlus m c
            (SIVar m) -> IVar m
            (SIVal v) -> IVal v
            _ -> error ("Parametro invalido: " ++ show i)
