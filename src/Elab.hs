{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Elab where
    
import Lang
import GHC.Utils.Misc

elabProg :: SProg -> Prog
elabProg (SProg sents trace) = Prog (map elabSent sents) trace

elabSent :: SSentence -> Sentence
elabSent (SAssign pRef p) = let
    params = sparams pRef
    name = sprocName pRef
    (normP, normParams) = runState (mapM elabParam params) p
    in (SAssign (SProcRef name normParams) (sproc2proc normP))

type IndexName = String

elabParam :: SParameter -> State SProc Parameter
elabParam par = do
    p' <- get
    let maxDiff = matchPar
            (\n _ -> maxIdxDiff n 0 p')
            (\n c -> maxIdxDiff n c p')
            (\n   -> maxIdxDiff n 0 p')
    let n = matchPar (const2 id) (const2 id) id
    when (maxDiff > 0) (put (normalizeIdx n maxDiff p'))
    matchPar
        (\n c -> return (Inductive n (c+maxDiff)))
        (\n c -> if maxDiff-c == 0 
                 then return Base n
                 else return (Inductive n (maxDiff-c)))
        (\n   -> if maxDiff == 0
                 then return Base n
                 else return (Inductive n maxDiff))
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
        maxIdxParam (Left (Index _)) acc = acc
        maxIdxParam (Left (IOp m "+" c)) acc = acc
        maxIdxParam (Left (IOp m "-" c)) acc = if n == m then max c acc else acc
        maxIdxParam (Right (SBase _)) acc = acc
        maxIdxParam (Right (SOp m "+" c)) acc = acc
        maxIdxParam (Right (SOp m "-" c)) acc = if n == m then max c acc else acc


normalizeIdx :: IndexName -> Int -> SProc -> SProc
normalizeIdx n diff p = go p
    where 
        go (SInternalChoice       p q) = (SInternalChoice (go p) (go q))
        go (SExternalChoice       p q) = (SExternalChoice (go p) (go q))
        go (SLabeledAlt           p q) = (SLabeledAlt     (go p) (go q))
        go (SParallel             p q) = (SParallel       (go p) (go q))
        go (SSequential           p q) = (SSequential     (go p) (go q))
        go (SPrefix     (Event m i) q) = (SPrefix (Event m (map normIdx i)) (go q))
        go (SByName (SProcRef m pars)) = (SByName (SProcRef m (map normPar pars)))
        go b = b
        normIdx (Index m) = if m == n then (IOp m "+" diff) else (Index m)
        normIdx (IOp m "+" c) = if m == n then (IOp m "+" (c+diff)) else (IOp m "+" c)
        normIdx (IOp m "-" c) = if m == n 
            then if diff-c>0 then (IOp m "+" (diff-c)) else (Index m)
            else (IOp m "-" c)
        normPar (SBase m) = if m == n then (SOp m "+" diff) else (SBase m)
        normPar (SOp m "+" c) = if m == n then (SOp m "+" (c+diff)) else (SOp m "+" c)
        normPar (SOp m "-" c) = if m == n
            then if diff-c>0 then (SOp m "+" (diff-c)) else (SBase m)
            else (SOp m "-" c)

-- `foldSProc f z proc` es identico a `foldr f z (inOrder proc)`
-- inOrder ignora los operadores y solo devuelve los eventos y
-- procesos (como eithers)
foldSProc :: (Either SIndex SProcRef -> b -> b) -> b -> SProc -> b
foldSProc f z (SInternalChoice p q) = foldSProc f (foldSProc f z q) p 
foldSProc f z (SExternalChoice p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SLabeledAlt p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SParallel p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SSequential p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SPrefix e q) = f (Left e) (foldSProc f z q)
foldSProc f z (SInterrupt e q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SByName n) = f (Right n) z
foldSProc f z SStop = z
foldSProc f z SSkip = z

sproc2proc :: SProc -> Proc
sproc2proc (SInternalChoice       p q) = InternalChoice (sproc2proc p) (sproc2proc q)
sproc2proc (SExternalChoice       p q) = ExternalChoice (sproc2proc p) (sproc2proc q)
sproc2proc (SLabeledAlt           p q) = LabeledAlt     (sproc2proc p) (sproc2proc q)
sproc2proc (SParallel             p q) = Parallel       (sproc2proc p) (sproc2proc q)
sproc2proc (SSequential           p q) = Sequential     (sproc2proc p) (sproc2proc q)
sproc2proc (SInterrupt            p q) = Interrupt      (sproc2proc p) (sproc2proc q)
sproc2proc (SPrefix               e p) = Prefix e p
sproc2proc (SByName (SProcRef n pars)) = (ByName
        (ProcRef n (map (\p -> case p of
                        (SOp m "+" c) -> Inductive m c
                        (SBase m) -> Base m) pars)))
sproc2proc                 SStop = Stop
sproc2proc                 SSkip = Skip
