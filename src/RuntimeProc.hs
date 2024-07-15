{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module RuntimeProc (
  initRuntime,
  RuntimeProc(..),
  compareProcs
)where

import AST
    ( Proc(..),
      ProcId,
      Sentence(..),
      Event,
      Prefix(Event),
      Generic(..) )
import RunnableProc (RunnableProc (..), ProcRep (..))
import Traces ( traces, generateRandomEvents, Trie (inTrie, mkTrie), TraceTrie, Trace (Trace, unTrace) )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (StdGen, RandomGen (genWord8))
import Control.Parallel (par)
import PrettyPrint (prettyPrint)
import Prettyprinter (Pretty(pretty), Doc, line, emptyDoc, vsep)
import Control.Monad.Trans.Accum (runAccum)
import Control.Monad.Accum (MonadAccum (add))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Data.List ( foldl', find )
import Data.Maybe


type Defines = Map.Map ProcId Proc
type AlphaSymbols = Map.Map ProcId [Event]
type Seen = Set.Set ProcId

data RuntimeProc = RuntimeProc { definitions :: Defines, runtimeRandom :: StdGen, alphaSymbols :: AlphaSymbols, runtimeProc :: Proc }
type Prog = [Sentence]

somethingUndefined :: Prog -> Bool
somethingUndefined prog = let
  defines = map (\case
                    (Assign idP _) -> idP
                    _ -> "") prog

  isDefined  (ByName idP) = idP `elem` defines
  isDefined  (ExternalChoice p q) = bothDefined p q
  isDefined  (InternalChoice p q) = bothDefined p q
  isDefined  (Parallel p q) = bothDefined p q
  isDefined  (Sequential p q) = bothDefined p q
  isDefined  (Prefix _ p) = isDefined p
  isDefined  (Interrupt p q) = bothDefined p q
  isDefined  Stop = True
  isDefined  Skip = True
  isDefined' (Assign _ p) = isDefined p
  isDefined' (Compare p q) = bothDefined p q

  bothDefined p q = isDefined p `par` isDefined q `par` (isDefined p && isDefined q)

  in
    not (all isDefined' prog)


alpha :: Defines -> Seen -> Proc -> [Event]
alpha ns seen (InternalChoice p q) = alpha ns seen p ++ alpha ns seen q
alpha ns seen (ExternalChoice p q) = alpha ns seen p ++ alpha ns seen q
alpha ns seen (Parallel p q) = alpha ns seen p ++ alpha ns seen q
alpha ns seen (Sequential p q) = alpha ns seen p ++ alpha ns seen q
alpha ns seen (Interrupt p q) = alpha ns seen p ++ alpha ns seen q
alpha ns seen (Prefix pref q) = let (Event ev) = pref in ev : alpha ns seen q
alpha ns seen (ByName p) = if p `Set.member` seen then [] else (case Map.lookup p ns of
    Just q -> alpha ns (Set.insert p seen) q
    Nothing -> error "Initialization error: Current process has an undefined process ( " ++ p ++ " )" `seq` [])
alpha _ _ Stop = []
alpha _ _ Skip = []

{- initRuntime inicializa todas las variables globales y luego devuelve una funcion constructura de procesos ejecutables -}
initRuntime :: StdGen -> Prog -> Maybe (Proc -> RuntimeProc)
initRuntime randomGen prog =
  if somethingUndefined prog
  then Nothing
  else let
    defines = foldl'
      (\m sent -> case sent of
        Assign pId p -> Map.insert pId p m
        Compare _ _ -> m)
      Map.empty prog
    alphaSym = Map.foldlWithKey'
      (\m sym p -> let alphaP = alpha defines (Set.singleton sym) p
                   in Map.insert sym alphaP m)
      Map.empty defines
    constructRuntime p = RuntimeProc {
        definitions = defines
      , runtimeRandom = randomGen
      , alphaSymbols = alphaSym
      , runtimeProc = p }
    in Just constructRuntime



instance RunnableProc RuntimeProc where
  run rt ev = fst3 (evalEvent rt ev NonDeterministic)
  accept rt ev = snd3 (evalEvent rt ev NonDeterministic)
  refusal rt ev = not (accept rt ev)

  getAlpha rt = alpha (definitions rt) Set.empty (runtimeProc rt)

  inAlpha rt ev =
    case runtimeProc rt of
      (InternalChoice p q) -> inAlpha (rt {runtimeProc = p}) ev ||
                              inAlpha (rt {runtimeProc = q}) ev
      (ExternalChoice p q) -> inAlpha (rt {runtimeProc = p}) ev ||
                              inAlpha (rt {runtimeProc = q}) ev
      (Parallel p q) -> inAlpha (rt {runtimeProc = p}) ev ||
                        inAlpha (rt {runtimeProc = q}) ev
      (Sequential p q) -> inAlpha (rt {runtimeProc = p}) ev ||
                          inAlpha (rt {runtimeProc = q}) ev
      (Interrupt p q) -> inAlpha (rt {runtimeProc = p}) ev ||
                        inAlpha (rt {runtimeProc = q}) ev
      (Prefix pref p) -> let (Event procEv) = pref in
                         procEv == ev || inAlpha (rt {runtimeProc = p}) ev
      (ByName p) -> case Map.lookup p (alphaSymbols rt) of
        Just alphaP -> ev `elem` alphaP
        Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ p ++ " )" `seq` False
      Stop -> False
      Skip -> False


  asProc rt =
    case runtimeProc rt of
      (InternalChoice p q) -> InternalChoiceRep (rt {runtimeProc = p}) (rt {runtimeProc = q})
      (ExternalChoice p q) -> ExternalChoiceRep (rt {runtimeProc = p}) (rt {runtimeProc = q})
      (Parallel p q) -> ParallelRep (rt {runtimeProc = p}) (rt {runtimeProc = q})
      (Sequential p q) -> SequentialRep (rt {runtimeProc = p}) (rt {runtimeProc = q})
      (Interrupt p q) -> InterruptRep (rt {runtimeProc = p}) (rt {runtimeProc = q})
      (Prefix pref p) -> let (Event ev) = pref in PrefixRep ev (rt {runtimeProc = p})
      (ByName p) -> ByNameRep p
      Stop -> StopRep
      Skip -> SkipRep


  findProc rt name = case Map.lookup name (definitions rt) of
    Just p -> rt {runtimeProc = p}
    Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ name ++ " )" `seq` rt

  fromProc (InternalChoiceRep p q) = p {runtimeProc = InternalChoice (runtimeProc p)
                                                                     (runtimeProc q)}
  fromProc (ExternalChoiceRep p q) = p {runtimeProc = ExternalChoice (runtimeProc p)
                                                                     (runtimeProc q)}
  fromProc (ParallelRep p q) = p {runtimeProc = Parallel (runtimeProc p) (runtimeProc q)}
  fromProc (InterruptRep p q) = p {runtimeProc = Interrupt (runtimeProc p) (runtimeProc q)}
  fromProc (SequentialRep p q) = p {runtimeProc = Sequential (runtimeProc p) (runtimeProc q)}
  fromProc (PrefixRep pref p) = p {runtimeProc = Prefix (Event pref) (runtimeProc p)}
  fromProc _ = error "Evaluation error: Trying to get environment from unexpected process representation"
  showProc p = show (runtimeProc p)

  accept' rt ev = snd3 (evalEvent rt ev Deterministic)

{- Data.Tuple.Extra define estas funciones pero no merece agregar una dependencia -}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

infixl 5 <|> -- operador para combinar evalEvent
(<|>) :: (a, b, Bool) -> (c, d, Bool) -> (a, b, Bool)
(<|>) (a, b, c) (_, _, d) = (a, b, c || d)

data Deterministic = NonDeterministic | Deterministic deriving Eq

{- Ya que recorremos el arbol, vamos a sacar toda la informacion posible -}
{- Returns: (run rt ev, accept rt ev, inAlpha rt ev) -}
evalEvent :: RuntimeProc -> Event -> Deterministic -> (RuntimeProc, Bool, Bool)
evalEvent rt_ ev_ deterministic = eval rt_ ev_
  where
  eval rt ev =
    case runtimeProc rt of
      (ExternalChoice q r) ->
        let q' = eval (rt {runtimeProc = q}) ev
            r' = eval (rt {runtimeProc = r}) ev
        in if
        | snd3 q' -> q' <|> r'
        | snd3 r' -> r' <|> q'
        | otherwise -> (rt, False, False) <|> q' <|> r'
      (InternalChoice q r) -> case deterministic of
          NonDeterministic ->
                let (internalChoice, gen') =  genWord8 $ runtimeRandom rt
                    q' = eval (rt {runtimeProc = q, runtimeRandom = gen'}) ev
                    r' = eval (rt {runtimeProc = r, runtimeRandom = gen'}) ev
                in if
                | even internalChoice && snd3 q' -> q' <|> r'
                | odd internalChoice && snd3 r' -> r' <|> q'
                | otherwise -> (rt, False, False) <|> q' <|> r'
          Deterministic ->
                let q' = eval (rt {runtimeProc = q}) ev
                    r' = eval (rt {runtimeProc = r}) ev
                in if
                | snd3 q' -> q' <|> r'
                | snd3 r' -> r' <|> q'
                | otherwise -> (rt, False, False) <|> q' <|> r'
      (Parallel q r) ->
        let q' = eval (rt {runtimeProc = q}) ev
            r' = eval (rt {runtimeProc = r}) ev
        in if
        | snd3 q' && snd3 r' -> (rt {
              runtimeProc = Parallel (runtimeProc $ fst3 q') (runtimeProc $ fst3 r')}, True, True)
        | snd3 q' && not (thd3 r') -> (rt {
              runtimeProc = Parallel (runtimeProc $ fst3 q') r}, True, True)
        | snd3 r' && not (thd3 q') -> (rt {
              runtimeProc = Parallel q (runtimeProc $ fst3 r')}, True, True)
        | otherwise -> (rt, False, False) <|> q' <|> r'
      (Sequential q r) ->
        let q' = eval (rt {runtimeProc = q}) ev
            r' = eval (rt {runtimeProc = r}) ev
        in case q of
          Stop -> (rt {runtimeProc = Stop}, False, False)
          Skip -> r'
          _ -> ((fst3 q') {runtimeProc = Sequential (runtimeProc $ fst3 q') r}
                , snd3 q', thd3 q') <|> r'
      (Interrupt q r) ->
        let q' = eval (rt {runtimeProc = q}) ev
            r' = eval (rt {runtimeProc = r}) ev
        in if
        | snd3 r' -> r' <|> q'
        | snd3 q' -> ((fst3 q') {runtimeProc = Interrupt (runtimeProc $ fst3 q') r}, True, True)
        | otherwise -> (rt, False, False) <|> q' <|> r'
      (Prefix pref q) -> let (Event procEv) = pref in
          if procEv == ev
          then (rt {runtimeProc = q}, True, True)
          else (rt, False, inAlpha (rt {runtimeProc = q}) ev)
      (ByName p) -> case Map.lookup p (definitions rt) of
          Just definition -> eval (rt {runtimeProc = definition}) ev
          Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ p ++ " )" `seq` (rt, False, False)
      Stop -> (rt, False, False)
      Skip -> (rt, False, False)

compareProcs :: RuntimeProc -> RuntimeProc -> (Bool, Doc AnsiStyle)
compareProcs rtA rtB = runAccum compareM emptyDoc
  where
    compareM =
      let
        randomGen = runtimeRandom rtA
        alphaA = Set.fromList (getAlpha rtA)
        alphaB = Set.fromList (getAlpha rtB)
        tests = generateRandomEvents rtA randomGen
        tracesA = concatMap (traces rtA) tests
        tracesB = concatMap (traces rtB) tests
        trieA = mkTrie tracesA :: TraceTrie
        trieB = mkTrie tracesB :: TraceTrie
      in if (alphaA == alphaB)
            && all (inTrie trieA) tracesB
      then return True
      else do
        add (pretty "Una comparacion ha fallado\n"
                  <>pretty "    "
                  <>prettyPrint (SentG (Compare (runtimeProc rtA) (runtimeProc rtB)))
                  <>line)
        if alphaA /= alphaB then do
          add (pretty "Alfabeto del lado izquierdo: "
                    <> pretty (commaSeparated alphaA)
                    <> line)
          add (pretty "Alfabeto del lado derecho: "
                    <> pretty (commaSeparated alphaB)
                    <> line)
        else do
        -- add (pretty tests <> line)
        -- add ((pretty . show) (mkTrie tracesA :: TraceTrie) <> line)
        -- add ((pretty . show) tracesA <> line)
        -- add ((pretty . show) (mkTrie tracesB :: TraceTrie) <> line)
        -- add ((pretty . show) tracesB <> line)
          let failure = fromJust (find (not . inTrie trieA) tracesB)
          add (pretty "Traza fallida: "
                    <> pretty (let (Trace failureEvents) = failure
                               in commaSeparated failureEvents)
                    <> line)
          add (pretty "Trazas validas del lado izquierdo:\n"
                    <> pretty trieA
                    <> line)
          add (pretty "Trazas validas del lado derecho:\n"
                    <> pretty trieB
                    <> line)
          add (pretty "Casos de prueba:\n"
                      <> vsep (map
                              (\t -> let
                                    trA = traces rtA t
                                    trB = traces rtB t
                                    in
                                    pretty "Test:"
                                    <> pretty (commaSeparated t)
                                    <> line
                                    <> pretty "Lado izquierdo:\n"
                                    <> vsep (map (pretty . commaSeparated . unTrace) trA)
                                    <> line
                                    <> pretty "Lado derecho:\n"
                                    <> vsep (map (pretty . commaSeparated . unTrace) trB)
                                    <> line)
                              tests))
        return False
    commaSeparated :: Foldable t => t String -> String
    commaSeparated = foldr
                     (\x acc -> if acc /= "" then x <> ", " <> acc else x)
                     ""
