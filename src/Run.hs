{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Run where

import Lang
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe
import Data.Functor

run :: Prog -> IO ()
run (Prog sents []) = do
    putStrLn "Especificacion cargada sin trazas para ejecutar."
    putStrLn "Ast:"
    mapM_ print sents
run (Prog sents tr) = do
    foldr
    return ()

data RunData =
    RunData {
        rSentences :: [Sentence],
        rEvent :: Event
    }

type MonadRun = WriterT [Event] (ExceptT String (Reader RunData))
newtype TraceEvent = TraceEvent { deTraceEvent :: Event }

-- Acciones de la monada
-- La referencia que se busca DEBE tener todos los parametros con valor.
-- Si todas las sentencias son cerradas entonces podemos asegurarlo.
getProc :: ProcRef -> MonadRun Proc
getProc pRef = do
    sents <- asks rSentences
    let  findProc = filter (matchRefInSent pRef) sents
    case findProc of
        [s] -> let (Assign _ p) = s in return p
        (_:_) -> throwError ("El proceso"++show pRef++" es ambiguo.")
        []    -> throwError ("El proceso "++show pRef++" no esta definido.")
    where
        matchRefInSent p (Assign q _) =
            if  (procName p == procName q) &&
                (length (params p) == length (params q))
            then ()
            else ()
        matchParams :: [Parameter] -> [Parameter] -> Writer [Parameter] Bool
        matchParams [Base v1] [Base v2] = 
            if (v1 == v2) then
                return True
            else
                return False
        matchParams [Base v1] [Inductive _ c] =
            if v1 - c >= 0 then do
                tell [Base (v1-c)]
                return True
            else
                return False
        matchParams ((Base v1):is1) ((Base v2):is2) = 
            if (v1 == v2) then 
                matchParams is1 is2
            else
                return False
        matchParams ((Base v1):is1) ((Inductive _ c):is2) =
            if v1 - c >= 0 then do
                tell [Base (v1-c)]
                matchParams is1 is2
            else
                return False
        matchParams _ _ = return False

-- InternalChoice con ocultacion de eventos tendria algo de significado
-- porque el procedimiento automatico eligiría uno de los procesos y lo
-- haría diferenciarse de ExternalChoice. Por ahora, son iguales. El
-- entorno decide en ambos casos.
accept :: Proc -> MonadRun (Maybe Proc)
accept (InternalChoice p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Just p', Just q') -> return (Just (InternalChoice p' q'))
        (Just p', Nothing) -> return (Just p')
        (Nothing, Just q') -> return (Just q')
        (Nothing, Nothing) -> return Nothing
accept (ExternalChoice p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Just p', Just q') -> return (Just (ExternalChoice p' q'))
        (Just p', Nothing) -> return (Just p')
        (Nothing, Just q') -> return (Just q')
        (Nothing, Nothing) -> return Nothing
accept alt@(LabeledAlt p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Just p', Nothing) -> return (Just p')
        (Nothing, Just q') -> return (Just q')
        (Nothing, Nothing) -> return Nothing
        (Just _, Just _) -> do
            ev <- asks rEvent
            throwError ("El evento "++show ev++" fue aceptado mas de una vez"++
                " en una alternativa: "++show alt)
accept (Sequential Skip q) = accept q
accept (Sequential p q) = do
    nextp <- accept p
    case nextp of
        Just p' -> return (Just (Sequential p' q))
        Nothing -> return Nothing
accept int@(Interrupt p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Just p', Nothing) -> return (Just (Interrupt p' q))
        (Nothing, Just q') -> return (Just q')
        (Nothing, Nothing) -> return Nothing
        (Just _, Just _) -> do
            ev <- asks rEvent
            throwError ("El evento "++show ev++" fue aceptado mas de una vez"++
                " en una interrupcion: "++show int)
accept (ByName n) = getProc n >>= accept
accept Stop = return Nothing
accept Skip = return Nothing
accept (Prefix e1 p) = do
    e2 <- asks rEvent
    if matchEvents e1 e2
        then return (Just p)
        else return Nothing
accept (Parallel p q) = do
    ev <- asks rEvent
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Nothing, Nothing) -> return Nothing
        (Just p', Nothing) -> do
            inQ <- inAlpha ev q
            if inQ
                then return Nothing
                else return (Just (Parallel p' q))
        (Nothing, Just q') -> do
            inP <- inAlpha ev p
            if inP
                then return Nothing
                else return (Just (Parallel p q'))
        (Just p', Just q') -> return (Just (Parallel p' q'))

-- Utilidades

inAlpha :: Event -> Proc -> MonadRun Bool
inAlpha e p = evalStateT (inAlpha' e p) []

inAlpha' :: Event -> Proc -> StateT [ProcRef] MonadRun Bool
inAlpha' e = go
    where
        go (InternalChoice p q) = bin p q
        go (ExternalChoice p q) = bin p q
        go (Parallel       p q) = bin p q
        go (Sequential     p q) = bin p q
        go (Interrupt      p q) = bin p q
        go (LabeledAlt     p q) = bin p q
        go Skip                 = return False
        go Stop                 = return False
        --
        go (Prefix        e1 q) =
            if matchEvents e e1 then
                return True
            else
                go q
        go (ByName           n) = do
            seen <- get
            if member n seen then
                return False
            else do
                put (n:seen)
                p <- lift (getProc n)
                go p
        bin p q = do
            x <- (go p)
            if x then
                return True
            else
                go q

-- SIEMPRE se matchea una traza contra un evento de especificacion
-- los eventos de las trazas tienen todos los indices valuados.
-- El resultado de matchear incluye el reemplazo de las variables
-- en el evento
-- 
matchEvents :: TraceEvent -> Event -> (Bool, [Index])
matchEvents (TraceEvent e1) e2 = 
    if  (eventName e1 == eventName e2) &&
        (length (indices e1) == length (indices e2))
    then runWriter (matchIndices (indices e1) (indices e2))
    else (False, [])
    where
        matchIndices :: [Index] -> [Index] -> Writer [Index] Bool
        matchIndices [IVal v1] [IVal v2] = 
            if (v1 == v2) then
                return True
            else
                return False
        matchIndices [IVal v1] [IVar _] = do
            tell [IVal v1]
            return True
        matchIndices [IVal (Int v1)] [IOp _ "+" c] =
            if v1 - c >= 0 then do
                tell [IVal (Int (v1-c))]
                return True
            else
                return False
        matchIndices ((IVal v1):is1) ((IVal v2):is2) = 
            if (v1 == v2) then 
                matchIndices is1 is2
            else
                return False
        matchIndices ((IVal v1):is1) ((IVar _):is2) = do
            tell [IVal v1]
            matchIndices is1 is2
        matchIndices ((IVal (Int v1)):is1) ((IOp _ "+" c):is2) =
            if v1 - c >= 0 then do
                tell [IVal (Int (v1-c))]
                matchIndices is1 is2
            else
                return False
        matchIndices _ _ = return False

-- Checks estaticos

-- Check que no hay ninguna parte de la sentencia con variables libres
checkClosed :: Sentence -> Bool
checkClosed s = False

-- Check que todas las alternativas son deterministas: Ninguna comparte eventos
checkLabeledAlt :: Proc -> Except String Bool
checkLabeledAlt = checkLA
    where
        checkLA (InternalChoice p q) = combine (checkLA p) (checkLA q)
        checkLA (ExternalChoice p q) = combine (checkLA p) (checkLA q)
        checkLA (Parallel       p q) = combine (checkLA p) (checkLA q)
        checkLA (Sequential     p q) = combine (checkLA p) (checkLA q)
        checkLA (Interrupt      p q) = combine (checkLA p) (checkLA q)
        checkLA (Prefix         _ q) = checkLA q
        checkLA alt@(LabeledAlt _ _) = (collectEvents alt <&> isJust)
        checkLA (ByName _) = return True
        checkLA Stop = return True
        checkLA Skip = return True
        collectEvents :: Proc -> Except String (Maybe [Event])
        collectEvents (LabeledAlt p q) = do
            evsP <- collectEvents p
            evsQ <- collectEvents q
            case (evsP, evsQ) of
                (Just evsP1, Just evsQ1) ->
                    let shared = any (`elem` evsQ1) evsP1 -- O(n^2). mejorar?
                    in if shared 
                        then return Nothing
                        else return (Just (evsP1++evsQ1))
                (_, _) -> return Nothing
        collectEvents (Prefix e _) = return (Just [e])
        collectEvents p = throwError ("Se esperaba un prefijo o alternativa"++
            " etiquetada, pero se encontro "++show p)
        combine p q = do
            vp <- p
            vq <- q
            return (vp && vq)
