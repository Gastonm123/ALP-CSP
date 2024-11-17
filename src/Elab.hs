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
    in (Assign (ProcRef name normParams) (shallowElabProc normP))

type IndexName = String

elabParam :: SParameter -> State SProc Parameter
elabParam par = do
    p' <- get
    let maxDiff = matchPar
            (\n _ -> maxIdxDiff n 0 p')
            (\n c -> maxIdxDiff n c p')
            (\n   -> maxIdxDiff n 0 p')
    let n = matchPar const const id
    when (maxDiff > 0) (put (normalizeIdxPar n maxDiff p'))
    matchPar
        (\m c -> return (Inductive m (c+maxDiff)))
        (\m c -> return (Inductive m (maxDiff-c)))
        (\m   -> Base m)
    where
        matchPar plus minus base = case par of
            (SOp n "+" c) -> plus n c
            (SOp n "-" c) -> minus n c
            (SBase n) -> base n
            _ -> error ("Parametro con operador invalido: " ++ show par)

-- calcula el mayor natural que se resta a cualquier indice o parametro
-- llamado `n`
maxIdxDiff :: IndexName -> Int -> SProc -> Int
maxIdxDiff n c sp = foldSProc maxIdxParam c sp
    where
        maxIdxParam (Left (IVar _)) acc = acc
        maxIdxParam (Left (IVal _)) acc = acc
        maxIdxParam (Left (IOp _ "+" _)) acc = acc
        maxIdxParam (Left (IOp m "-" c)) acc = if n == m then max c acc else acc
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
        go (SPrefix     (Event m i) q) = (SPrefix (Event m (map normIdx i)) (go q))
        go (SByName (SProcRef m pars)) = (SByName (SProcRef m (map normPar pars)))
        go b = b
        normIdx idx@(IVal  _) = idx
        normIdx (IVar      m) = if m == n then (IOp m "+" diff) else (IVar m)
        normIdx (IOp m "+" c) = if m == n then (IOp m "+" (c+diff)) else (IOp m "+" c)
        normIdx (IOp m "-" c) = if m == n
            then if diff-c>0 then (IOp m "+" (diff-c)) else (IVar m)
            else (IOp m "-" c)
        normPar (SBase m) = if m == n then (SOp m "+" diff) else (SBase m)
        normPar (SOp m "+" c) = if m == n then (SOp m "+" (c+diff)) else (SOp m "+" c)
        normPar (SOp m "-" c) = if m == n
            then if diff-c>0 then (SOp m "+" (diff-c)) else (SBase m)
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
foldSProc f z (SPrefix    (Event _ is) q) = foldr (f . Left) (foldSProc f z q) is
foldSProc f z (SByName (SProcRef _ pars)) = foldr (f . Right) z pars
foldSProc _ z SStop = z
foldSProc _ z SSkip = z

shallowElabProc :: SProc -> Proc
shallowElabProc = go
    where
        go (SInternalChoice          p q) = InternalChoice (go p) (go q)
        go (SExternalChoice          p q) = ExternalChoice (go p) (go q)
        go (SLabeledAlt              p q) = LabeledAlt     (go p) (go q)
        go (SParallel                p q) = Parallel       (go p) (go q)
        go (SSequential              p q) = Sequential     (go p) (go q)
        go (SInterrupt               p q) = Interrupt      (go p) (go q)
        go (SPrefix                  e p) = Prefix e (go p)
        go pr@(SByName (SProcRef n pars)) = ByName (ProcRef n (map (shallowElabPar pr) pars))
        go                          SStop = Stop
        go                          SSkip = Skip
        shallowElabPar pr p = case p of
            (SOp m "+" c) -> Inductive m c
            (SBase m) -> Base m
            _ -> error ("Fallo en la elaboracion del proceso " ++ (show pr))
