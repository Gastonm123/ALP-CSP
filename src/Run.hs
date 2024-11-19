{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Run where

import Lang
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe

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

type MonadRun a = WriterT [Event] (ExceptT String (Reader RunData)) a

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
            throwError ("El evento "++show ev++" fue aceptado mas de una vez en una alternativa: "++show alt)
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
            throwError ("El evento "++show ev++" fue aceptado mas de una vez en una interrupcion: "++show int)
accept (ByName n) = do
    sents <- asks rSentences
    let  findProc = filter matchRefInSent sents
    case findProc of
        [s] -> let (Assign _ p) = s in accept p
        (_:_) -> throwError ("El proceso"++show n++" es ambiguo.")
        []    -> throwError ("El proceso "++show n++" no esta definido.")
accept Stop = return Nothing
accept Skip = return Nothing
accept (Prefix e p) = throwError "matchear eventos......."
accept (Parallel p q) = throwError "dificil........"

-- Checks

-- Check que todas las alternativas son deterministas: Ninguna comparte eventos
checkLabeledAlt :: Proc -> Except String Bool
checkLabeledAlt (InternalChoice p q) = (checkLabeledAlt p) && (checkLabeledAlt q)
checkLabeledAlt (ExternalChoice p q) = (checkLabeledAlt p) && (checkLabeledAlt q)
checkLabeledAlt (Parallel       p q) = (checkLabeledAlt p) && (checkLabeledAlt q)
checkLabeledAlt (Sequential     p q) = (checkLabeledAlt p) && (checkLabeledAlt q)
checkLabeledAlt (Interrupt      p q) = (checkLabeledAlt p) && (checkLabeledAlt q)
checkLabeledAlt (Prefix         e q) = checkLabeledAlt q
checkLabeledAlt alt@(LabeledAlt p q) = isJust (collectEvents alt)
checkLabeledAlt (ByName p) = True
checkLabeledAlt Stop = True
checkLabeledAlt Skip = True
    where
        collectEvents (LabeledAlt p q) = do
            evsP <- collectEvents p
            evsQ <- collectEvents q
            let shared = any (map (`elem` evsQ) evsP) -- O(n^2). mejorar?
            if shared then Nothing else Just (evsP++evsQ)
        collectEvents (Prefix e p) = Just e
        collectEvents p = throwError ("Se esperaba un prefijo o alternativa etiquetada, pero se encontro "++show p)