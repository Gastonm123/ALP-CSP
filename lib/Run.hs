{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Run (run, accept, Conf(..)) where
{- Exporta run para ejecucion desde CLI y accept para modo interactivo -}

import Lang
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe
import Data.List
import Data.Functor
import Control.Monad
import Debug.Trace (trace, traceM, traceShow, traceShowM)
import PrettyPrint
import Prettyprinter
import GHC.Utils.Monad


run :: Prog -> ReaderT Conf IO ()
run (Prog [] _) = do
    liftIO (putStrLn "! Error: El parser generó una especificacion vacía")
run (Prog sents []) = do
    case last sents of
        (Assign _ espec) -> do
            liftIO (putStrLn "> Especificacion cargada sin trazas para ejecutar.")
            liftIO (putStrLn "> Especificacion:")
            liftIO (render (prettyPrint espec))
        (Limit _ _) -> do
            liftIO (putStrLn ("! Error: la ultima sentencia del archivo debe ser"
                ++" una asignacion"))
run (Prog sents tr) = do
    case last sents of
        (Assign _ espec) -> do
            {- La ultima declaracion del archivo es la especificacion -}
            ((result, acceptedEvs), espec1) <-
                (runStateT . runWriterT . runExceptT)
                    (mapM_ handleEvent tr) espec
            {- Diversion con monadas :D -}
            case result of
                (Left  (err, razones)) -> do
                        liftIO (putStrLn ("! Error: "++err))
                        unless (null razones) (do
                            liftIO (putStrLn "! Razon:")
                            liftIO ((render . indent 4 . vsep . map pretty)
                                    razones))
                        liftIO (putStrLn ("! Eventos procesados: "
                            ++ ((intercalate ", " . map show) acceptedEvs)))
                (Right _)   -> do
                        isDebug <- asks debug
                        when (isDebug) (do
                            liftIO (putStrLn "? Estado final de la especificacion:")
                            liftIO (render (prettyPrint espec1)))
                        liftIO (putStrLn  "> OK!")
                        liftIO (putStrLn ("> La especificacion acepto todos los"
                            ++ " eventos de la traza"))
        (Limit _ _) -> do
            liftIO (putStrLn ("! Error: la ultima sentencia del archivo debe ser"
                ++" una asignacion"))
    where
        handleEvent :: Event -> MonadHandler ()
        handleEvent trEvent = do
            espec1 <- get
            tell [trEvent]
            isDebug <- asks debug
            when isDebug (do
                liftIO (putStrLn ("? Siguiente evento: "++show trEvent))
                liftIO (putStrLn ("? Estado especificacion:"))
                liftIO (render (prettyPrint espec1))
                liftIO (putStrLn ""))
            let accOrFailed = (runReader . runExceptT)
                    (accept espec1) (RunData sents trEvent)
            case accOrFailed of
                Left err -> throwError (err, [])
                Right (Left razones) -> throwError ("Evento rechazado ("
                        ++show trEvent++")", razones)
                Right (Right espec2) -> put espec2

data RunData =
    RunData {
        rSentences :: [Sentence],
        rEvent :: Event
    }

type MonadRun = ExceptT String (Reader RunData)

type MonadHandler =
        ExceptT (String, [Razon])
        (WriterT [Event]
        (StateT Proc
        (ReaderT Conf IO)))

newtype TraceEvent = TraceEvent { deTraceEvent :: Event }

data SubstLoop a = SubstLoop { substProc :: Proc, substVars :: [a] }

data Conf =
    Conf {
        debug :: Bool
    }

type Razon = String

accept :: Proc -> MonadRun (Either [Razon] Proc)
accept (InternalChoice p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Right p', Right q') -> return (Right (InternalChoice p' q'))
        (Right p', Left _) -> return (Right p')
        (Left _, Right q') -> return (Right q')
        (Left r1, Left r2) -> return (Left (r1++r2))
{-  InternalChoice con ocultacion de eventos tendria mas sentido porque el
    procedimiento automatico eligiría uno de los procesos. Por ahora External e
    Internal son iguales. El entorno decide en ambos casos. -}
accept (ExternalChoice p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Right p', Right q') -> return (Right (ExternalChoice p' q'))
        (Right p', Left _) -> return (Right p')
        (Left _, Right q') -> return (Right q')
        (Left r1, Left r2) -> return (Left (r1++r2))
accept alt@(LabeledAlt p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Right p', Left _) -> return (Right p')
        (Left _, Right q') -> return (Right q')
        (Left r1, Left r2) -> return (Left (r1++r2))
        (Right _, Right _) -> do
            ev <- asks rEvent
            throwError ("El evento "++show ev++" fue aceptado mas de una vez"++
                " en una alternativa: "++(show . prettyPrint) alt)
accept (Sequential Stop _) = return (Left ["Se alcanzo STOP"])
accept (Sequential p Skip) = accept p
accept (Sequential Skip p) = accept p
accept (Sequential p q) = do
    nextp <- accept p
    case nextp of
        Right p' -> return (Right (Sequential p' q))
        Left r -> return (Left r)
accept int@(Interrupt p q) = do
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Right p', Left _) -> return (Right (Interrupt p' q))
        (Left _, Right q') -> return (Right q')
        (Left r1, Left r2) -> return (Left (r1++r2))
        (Right _, Right _) -> do
            ev <- asks rEvent
            throwError ("El evento "++show ev++" fue aceptado mas de una vez"++
                " en una interrupcion: "++(show . prettyPrint) int)
accept (ByName n) = do
    p <- getProc n
    accept p
accept Stop = return (Left ["Se alcanzo STOP"])
accept Skip = return (Left ["Se alcanzo SKIP"])
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
            return (Right p1)
        (False, _) -> return (Left [{- sin razon en especifico -}])
accept (Parallel Stop p) = accept p
accept (Parallel p Stop) = accept p
accept (Parallel Skip p) = accept p
accept (Parallel p Skip) = accept p
accept (Parallel p q) = do
    ev <- asks rEvent
    nextp <- accept p
    nextq <- accept q
    case (nextp, nextq) of
        (Left r1, Left r2) -> return (Left (r1++r2))
        (Right p', Left r) -> do
            inQ <- inAlpha ev q
            if inQ then do
                return (Left (r ++ [("Falla de sincronizacion (bloqueado por "
                        ++(show . prettyPrint) q++")")]))
            else case q of
                Stop -> return (Right p')
                Skip -> return (Right p')
                _    -> return (Right (Parallel p' q))
        (Left r, Right q') -> do
            inP <- inAlpha ev p
            if inP then do
                return (Left (r ++ [("Falla de sincronizacion (bloqueado por "
                        ++(show . prettyPrint) p++")")]))
            else case p of
                Stop -> return (Right q')
                Skip -> return (Right q')
                _    -> return (Right (Parallel p q'))
        (Right p', Right q') -> return (Right (Parallel p' q'))

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
                    {-let
                    is = indices e1
                    varsOfEvent = mapMaybe asVariable is
                    substLoop = (forM_ varsOfEvent (\n -> do
                        p1   <- gets substProc
                        vars <- gets substVars
                        let p2 = subst n (head vars) p1
                        put (SubstLoop p2 (tail vars)))
                        {- no es posible fallar en esta parte por head o tail
                        porque la lista de reemplazos es infinita -}
                        ) :: State (SubstLoop Int) ()
                    substData = (SubstLoop q (repeat (-1)))
                    (SubstLoop p1 _) = execState substLoop substData
                    in go p1-}
                {- Marcamos todos los parametros ligados al evento como:
                -    ab.i -> NT.i
                - donde el parametro i esta ligado al evento ab.i
                -}
        go (ByName           n) = do
            seen <- get
            let pars = params n
            if n `elem` seen then
                return False
            else if any isInductive pars then do
                put (n:seen)
                sents <- asks rSentences
                let matches = filter (\s -> case s of
                        (Assign ref _) -> n == ref
                        (Limit  ref _) -> n == ref) sents
                let flat = map (\s -> case s of
                        (Assign ref p) -> (ref, p)
                        (Limit  ref p) -> (ref, p)) matches
                -- para cada match:
                --      para cada parametro no inductivo de n:
                --          si el parametro respectivo en el match es inductivo:
                --              sustituir en el proceso el parametro por el
                --              valor en n
                let procs1 = map (\(ref, p) -> execState
                        ((forM_ (zip pars (params ref)) 
                            (\(parOfN, parOfMatch) -> do
                                p1 <- get
                                case (parOfN, parOfMatch) of
                                    (Base v, Inductive m c) ->
                                        put (subst m (v-c) p1)
                                    _ -> return ())) :: State Proc ())
                                p)
                        flat
                anyM go procs1
                -- buscar en los alfabetos alcanzables
            else do
                put (n:seen)
                p <- lift (getProc n)
                go p
        -- asProc (Assign _ p) = p
        -- asProc (Limit  _ p) = p
        isInductive (Inductive _ _) = True
        isInductive (Base _) = False
        {-asVariable (IVar v) = Just v
        asVariable (IPlus v _) = Just v 
        asVariable (IVal _) = Nothing-}

-- La referencia que se busca DEBE tener todos los parametros con valor.
-- Si todas las sentencias son cerradas entonces podemos asegurarlo.
getProc :: ProcRef -> MonadRun Proc
getProc pRef = do
    -- traceM (show pRef)
    sents <- asks rSentences
    let matchs = findProc sents
    let limits = filter (\(s, _) -> isLimit s) matchs
    if not (null limits) then
        case limits of
            [(Limit pRef1 p, pars)] -> do
                -- traceM (show s)
                let parsOfMatch = params pRef1
                let substLoop = (forM_ parsOfMatch (\i -> case i of
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
            (_:_) -> throwError ("El limite del proceso "++show pRef++" es ambiguos.")
            []    -> throwError ("El limite del proceso "++show pRef++" no esta definido.")
    else
        case matchs of
            [(Assign pRef1 p, pars)] -> do
                -- traceM (show s)
                let parsOfMatch = params pRef1
                let substLoop = (forM_ parsOfMatch (\i -> case i of
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
        findProc = mapMaybe (\s -> case s of
            (Assign q _) -> matchRefs pRef q >>= (\pars -> return (s, pars))
            (Limit  q _) -> matchRefs pRef q >>= (\pars -> return (s, pars)))
        matchRefs p q =
            if (procName p == procName q) &&
               (length (params p) == length (params q)) then
                if null (params p) then
                    Just []
                else case runWriter (matchParams (params p) (params q)) of
                    (True, pars) -> Just pars
                    (False,   _) -> Nothing
            else
                Nothing
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
        isLimit (Limit  _ _) = True
        isLimit (Assign _ _) = False

-- SIEMPRE se matchea una traza contra un evento de especificacion.
-- Los eventos de las trazas tienen todos los indices valuados.
-- El resultado de matchear incluye el reemplazo de las variables
-- en el evento
-- 
matchEvents :: TraceEvent -> Event -> (Bool, [Val])
matchEvents (TraceEvent e1) e2 =
    if (eventName e1 == eventName e2) &&
       (length (indices e1) == length (indices e2)) then
        if null (indices e1) then
            (True, [])
        else
            runWriter (matchIndices (indices e1) (indices e2))
    else
        (False, [])
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
