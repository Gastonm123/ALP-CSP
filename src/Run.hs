{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Run (run, accept) where
{- Exporta run para ejecucion desde CLI y accept para modo interactivo -}

import Lang
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe
import Data.Functor
import Control.Monad

run :: Prog -> IO ()
run (Prog [] _) = do
    putStrLn "! Error: El parser generó un programa vacío"
run (Prog sents []) = do
    putStrLn "> Especificacion cargada sin trazas para ejecutar."
    putStrLn "> Ast:"
    mapM_ print sents
run (Prog sents tr) = do
    let (Assign _ espec) = head (reverse sents)
    {- La ultima declaracion del archivo es la especificacion -}
    let (result, acceptedEvs) = (evalState . runWriterT . runExceptT)
            (mapM_ handleEvent tr) espec
    {- Diversion con monadas :D -}
    case result of
        (Left  err) -> do
                putStrLn ("! Error: "++err)
                putStrLn ("! Eventos aceptados: "++show acceptedEvs)
        (Right _)   -> do
                putStrLn  "> OK!"
                putStrLn ("> La especificacion acepto todos los eventos de la"
                    ++" traza")
    where
        handleEvent :: Event -> ExceptT String (WriterT [Event] (State Proc)) ()
        handleEvent trEvent = do
            espec1 <- get
            tell [trEvent]
            let accOrFailed = (runReader . runExceptT)
                    (accept espec1) (RunData sents trEvent)
            case accOrFailed of
                Left err -> throwError err
                Right Nothing -> throwError ("Event rejected "++show trEvent)
                Right (Just espec2) -> put espec2

data RunData =
    RunData {
        rSentences :: [Sentence],
        rEvent :: Event
    }

type MonadRun = ExceptT String (Reader RunData)

newtype TraceEvent = TraceEvent { deTraceEvent :: Event }

data SubstLoop a = SubstLoop { substProc :: Proc, substVars :: [a] }

-- Acciones de la monada

-- La referencia que se busca DEBE tener todos los parametros con valor.
-- Si todas las sentencias son cerradas entonces podemos asegurarlo.
getProc :: ProcRef -> MonadRun Proc
getProc pRef = do
    sents <- asks rSentences
    case findProc sents of
        [s] -> do
            let (Assign _ p, pars) = s
            let parsOfRef = params pRef
            let substLoop = (forM_ parsOfRef (\i -> case i of
                    (Inductive n _) -> do
                        p1 <- gets substProc
                        vars <- gets substVars
                        let p2 = subst n (head vars) p1
                        put (SubstLoop p2 (tail vars))
                        {- no es posible fallar en esta parte por head o tail
                           porque los reemplazos para las variables se
                           obtuvieron uno por uno desde los parametros -}
                    (Base _) -> return ())) :: State (SubstLoop Int) ()
            let (SubstLoop p1 _) = execState substLoop (SubstLoop p pars)
            return p1
        (_:_) -> throwError ("El proceso "++show pRef++" es ambiguo.")
        []    -> throwError ("El proceso "++show pRef++" no esta definido.")
    where
        findProc (sent:ss) = case matchRefInSent pRef sent of
            (True, pars) -> (sent, pars) : findProc ss
            (False, _) -> findProc ss
        findProc [] = []
        matchRefInSent p (Assign q _) =
            if  (procName p == procName q) &&
                (length (params p) == length (params q))
            then runWriter (matchParams (params p) (params q))
            else (False, [])
        matchParams :: [Parameter] -> [Parameter] -> Writer [Int] Bool
        matchParams [Base v1] [Base v2] =
            if (v1 == v2) then
                return True
            else
                return False
        matchParams [Base v1] [Inductive _ c] =
            if v1 - c >= 0 then do
                tell [v1-c]
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
                tell [v1-c]
                matchParams is1 is2
            else
                return False
        matchParams _ _ = return False


accept :: Proc -> MonadRun (Maybe Proc)
accept (InternalChoice p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Just p', Just q') -> return (Just (InternalChoice p' q'))
        (Just p', Nothing) -> return (Just p')
        (Nothing, Just q') -> return (Just q')
        (Nothing, Nothing) -> return Nothing
{-  InternalChoice con ocultacion de eventos tendria mas sentido porque el
    procedimiento automatico eligiría uno de los procesos. Por ahora External e
    Internal son iguales. El entorno decide en ambos casos. -}
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
    case matchEvents (TraceEvent e2) e1 of
        (True, is) -> do
            let isOfPrefix = indices e1
            let substLoop = (forM_ isOfPrefix (\i -> case i of
                    (IPlus n _) -> do
                        p1   <- gets substProc
                        vars <- gets substVars
                        guard (not (null vars))
                        p2   <- lift (subst n (head vars) p1)
                        put (SubstLoop p2 (tail vars))
                    (IVar n) -> do
                        p1   <- gets substProc
                        vars <- gets substVars
                        guard (not (null vars))
                        {- no es posible fallar en esta parte porque los
                           reemplazos para las variables se obtuvieron uno por
                           uno desde los indices -}
                        p2   <- lift (subst n (head vars) p1)
                        put (SubstLoop p2 (tail vars))
                    (IVal _) -> return ())) :: StateT (SubstLoop Val) MonadRun ()
            (SubstLoop p1 _) <- execStateT substLoop (SubstLoop p is)
            return (Just p1)
        (False, _) -> return Nothing
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
        go :: Proc -> StateT [ProcRef] MonadRun Bool
        go (InternalChoice p q) = (||) <$> go p <*> go q
        go (ExternalChoice p q) = (||) <$> go p <*> go q
        go (Parallel       p q) = (||) <$> go p <*> go q
        go (Sequential     p q) = (||) <$> go p <*> go q
        go (Interrupt      p q) = (||) <$> go p <*> go q
        go (LabeledAlt     p q) = (||) <$> go p <*> go q
        go Skip                 = return False
        go Stop                 = return False
        --
        go (Prefix        e1 q) =
            case matchEvents (TraceEvent e) e1 of
                (True,  _) -> return True
                (False, _) -> go q
        go (ByName           n) = do
            seen <- get
            if n `elem` seen then
                return False
            else do
                put (n:seen)
                p <- lift (getProc n)
                go p

-- SIEMPRE se matchea una traza contra un evento de especificacion.
-- Los eventos de las trazas tienen todos los indices valuados.
-- El resultado de matchear incluye el reemplazo de las variables
-- en el evento
-- 
matchEvents :: TraceEvent -> Event -> (Bool, [Val])
matchEvents (TraceEvent e1) e2 =
    if  (eventName e1 == eventName e2) &&
        (length (indices e1) == length (indices e2))
    then runWriter (matchIndices (indices e1) (indices e2))
    else (False, [])
    where
        matchIndices :: [Index] -> [Index] -> Writer [Val] Bool
        matchIndices [IVal v1] [IVal v2] =
            if (v1 == v2) then
                return True
            else
                return False
        matchIndices [IVal v1] [IVar _] = do
            tell [v1]
            return True
        matchIndices [IVal (Int v1)] [IPlus _ c] =
            if v1 - c >= 0 then do
                tell [Int (v1-c)]
                return True
            else
                return False
        matchIndices ((IVal v1):is1) ((IVal v2):is2) =
            if (v1 == v2) then
                matchIndices is1 is2
            else
                return False
        matchIndices ((IVal v1):is1) ((IVar _):is2) = do
            tell [v1]
            matchIndices is1 is2
        matchIndices ((IVal (Int v1)):is1) ((IPlus _ c):is2) =
            if v1 - c >= 0 then do
                tell [Int (v1-c)]
                matchIndices is1 is2
            else
                return False
        matchIndices _ _ = return False

class VariableSubstitution a ret where
    subst :: String -> a -> Proc -> ret

instance VariableSubstitution Int Proc where
    subst n vn = go
        where
            go (InternalChoice p q) = InternalChoice (go p) (go q)
            go (ExternalChoice p q) = ExternalChoice (go p) (go q)
            go (LabeledAlt     p q) = LabeledAlt (go p) (go q)
            go (Parallel       p q) = Parallel (go p) (go q)
            go (Sequential     p q) = Sequential (go p) (go q)
            go (Interrupt      p q) = Interrupt (go p) (go q)
            go (Prefix         e q) = let
                    nameE = eventName e
                    indE  = indices e
                    indE1 = map (\i -> case i of
                        IVar m -> if m == n then IVal (Int vn) else IVar m
                        IPlus m c -> if m == n then IVal (Int (vn+c)) else IPlus m c
                        val -> val) indE
                    in Prefix (Event nameE indE1) (go q)
            go (ByName          pr) = let
                    namePr  = procName pr
                    parsPr  = params pr
                    parsPr1 = map (\i -> case i of
                        Inductive m c ->
                            if m == n then
                                Base (vn + c)
                            else
                                Inductive m c
                        val -> val) parsPr
                    in ByName (ProcRef namePr parsPr1)
            go Stop = Stop
            go Skip = Skip

instance VariableSubstitution Val (MonadRun Proc) where
    subst n vn = go
        where
            go (InternalChoice p q) = InternalChoice <$> (go p) <*> (go q)
            go (ExternalChoice p q) = ExternalChoice <$> (go p) <*> (go q)
            go (LabeledAlt     p q) = LabeledAlt <$> (go p) <*> (go q)
            go (Parallel       p q) = Parallel <$> (go p) <*> (go q)
            go (Sequential     p q) = Sequential <$> (go p) <*> (go q)
            go (Interrupt      p q) = Interrupt <$> (go p) <*> (go q)
            go (Prefix         e q) = do
                    let nameE = eventName e
                    let indE  = indices e
                    indE1 <- mapM (\i -> case i of
                        IVar m ->
                            if m == n then
                                return (IVal vn)
                            else
                                return (IVar m)
                        IPlus m c ->
                            if m == n then
                                case vn of
                                    (Int  v) -> return (IVal (Int (v+c)))
                                    (Char _) -> typeError1 e
                            else return (IPlus m c)
                        val -> return val) indE
                    (Prefix (Event nameE indE1)) <$> (go q)
            go (ByName          pr) = do
                    let namePr  = procName pr
                    let parsPr  = params pr
                    parsPr1 <- mapM (\i -> case i of
                        Inductive m c ->
                            if m == n then
                                case vn of
                                    (Int  v) -> return (Base (v + c))
                                    (Char _) -> typeError pr
                            else
                                return (Inductive m c)
                        val -> return val) parsPr
                    return (ByName (ProcRef namePr parsPr1))
            go Stop = return Stop
            go Skip = return Skip
            typeError :: ProcRef -> MonadRun a
            typeError pr = throwError ("Se intento reemplazar el parametro "
                ++n++" en el proceso "++show pr++" con un caracter")
            typeError1 :: Event -> MonadRun a
            typeError1 ev = throwError ("Se intento reemplazar el indice "
                ++n++" en el evento "++show ev++" con un caracter, pero este"
                ++" debe ser operado con un numero")

-- Checks estaticos

-- Check que no hay ninguna parte de la sentencia con variables libres
checkClosed :: Sentence -> Bool
checkClosed s = False

-- Check que todas las alternativas son deterministas: Ninguna comparte eventos
checkLabeledAlt :: Proc -> Except String Bool
checkLabeledAlt = checkLA
    where
        checkLA (InternalChoice p q) = (&&) <$> (checkLA p) <*> (checkLA q)
        checkLA (ExternalChoice p q) = (&&) <$> (checkLA p) <*> (checkLA q)
        checkLA (Parallel       p q) = (&&) <$> (checkLA p) <*> (checkLA q)
        checkLA (Sequential     p q) = (&&) <$> (checkLA p) <*> (checkLA q)
        checkLA (Interrupt      p q) = (&&) <$> (checkLA p) <*> (checkLA q)
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
                    -- shared = O(n^2). mejorar?
                    let shared = any (\e -> e `elem` evsQ1) evsP1
                    in if shared
                        then return Nothing
                        else return (Just (evsP1++evsQ1))
                (_, _) -> return Nothing
        collectEvents (Prefix e _) = return (Just [e])
        collectEvents p = throwError ("Se esperaba un prefijo o alternativa"++
            " etiquetada, pero se encontro "++show p)
