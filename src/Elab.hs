module Elab where
    
import Lang
import GHC.Utils.Misc

elab :: SProg -> Prog
elab (SProg sents trace) = Prog (map elabSent sents) trace

elabSent :: SSentence -> Sentence
elabSent (SAssign pRef p) = let
    params = sparams pRef
    forM params elabParam
    return params

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
normalizeIdx (SInternalChoice)


-- `foldSProc f z proc` es identico a `foldr f z (inOrder proc)`
-- inOrder ignora los operadores y solo devuelve los eventos y
-- procesos (como eithers)
foldSProc :: (Either SIndex SProcRef -> b -> b) -> b -> SProc -> b
foldSProc f z (SInternalChoice p q) = foldSProc f (foldSProc f z q) p 
foldSProc f z (SExternalChoice p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SLabeledAlt p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SParallel p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SSequential p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SPrefix p q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SPrefix e q) = f (Left e) (foldSProc f z q)
foldSProc f z (SInterrupt e q) = foldSProc f (foldSProc f z q) p
foldSProc f z (SByName n) = f (Right n) z
foldSProc f z SStop = z
foldSProc f z SSkip = z