{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}
module RuntimeProc (
  initRuntime,
  RuntimeProc(..)
)where

import AST
import RunnableProc (RunnableProc (..), ProcRep (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (StdGen, RandomGen (genWord8))
import Control.Parallel (par)
import Data.Foldable (Foldable(foldl'))

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
alpha ns seen (Prefix pref q) = pref : alpha ns seen q
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

{- Data.Tuple.Extra define estas funciones pero no merece agregar una dependencia -}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

infixl 5 <|> -- operador para combinar run'
(<|>) :: (a, b, Bool) -> (c, d, Bool) -> (a, b, Bool)
(<|>) (a, b, c) (_, _, d) = (a, b, c || d)

        
instance RunnableProc RuntimeProc where
  run rt ev = fst3 (run' rt ev)
  accept rt ev = not (refusal rt ev)
  refusal rt ev = snd3 (run' rt ev)
  
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
      (Prefix pref p) -> pref == ev || inAlpha (rt {runtimeProc = p}) ev
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
      (Prefix pref p) -> PrefixRep pref (rt {runtimeProc = p})
      (ByName p) -> ByNameRep p
      Stop -> StopRep
      Skip -> SkipRep


  findProc rt name = case Map.lookup name (definitions rt) of
    Just p -> rt {runtimeProc = p}
    Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ name ++ " )" `seq` rt

{- Ya que recorremos el arbol, vamos a sacar toda la informacion posible -}
{- Returns: (run rt ev, accept rt ev, inAlpha rt ev) -}
run' :: RuntimeProc -> Event -> (RuntimeProc, Bool, Bool)
run' rt ev =
  case runtimeProc rt of
    (ExternalChoice q r) ->
      let q' = run' (rt {runtimeProc = q}) ev
          r' = run' (rt {runtimeProc = r}) ev
      in if
      | snd3 q' -> q' <|> r'
      | snd3 r' -> r' <|> q'
      | otherwise -> (rt, False, False) <|> q' <|> r'
    (InternalChoice q r) ->
      let (internal, gen') =  genWord8 $ runtimeRandom rt
          q' = run' (rt {runtimeProc = q, runtimeRandom = gen'}) ev
          r' = run' (rt {runtimeProc = r, runtimeRandom = gen'}) ev
      in if
      | even internal && snd3 q' -> q' <|> r'
      | odd internal && snd3 r' -> r' <|> q'
      | otherwise -> (rt, False, False) <|> q' <|> r'
    (Parallel q r) ->
      let q' = run' (rt {runtimeProc = q}) ev
          r' = run' (rt {runtimeProc = r}) ev
      in if
      | snd3 q' && snd3 r' -> (rt {
            runtimeProc = Parallel (runtimeProc $ fst3 q') (runtimeProc $ fst3 r')}, True, True)
      | snd3 q' && not (thd3 r') -> (rt {
            runtimeProc = Parallel (runtimeProc $ fst3 q') r}, True, True)
      | snd3 r' && not (thd3 q') -> (rt {
            runtimeProc = Parallel q (runtimeProc $ fst3 r')}, True, True)
      | otherwise -> (rt, False, False) <|> q' <|> r'
    (Sequential q r) ->
      let q' = run' (rt {runtimeProc = q}) ev
          r' = run' (rt {runtimeProc = r}) ev
      in case q of
        Stop -> (rt {runtimeProc = Stop}, False, False)
        Skip -> r'
        _ -> ((fst3 q') {runtimeProc = Sequential (runtimeProc $ fst3 q') r}
              , snd3 q', thd3 q') <|> r'
    (Interrupt q r) ->
      let q' = run' (rt {runtimeProc = q}) ev
          r' = run' (rt {runtimeProc = r}) ev
      in if
      | snd3 r' -> r' <|> q'
      | snd3 q' -> ((fst3 q') {runtimeProc = Interrupt (runtimeProc $ fst3 q') r}, True, True)
      | otherwise -> (rt, False, False) <|> q' <|> r'
    (Prefix pref q) -> if pref == ev 
        then (rt {runtimeProc = q}, True, True)
        else (rt, False, inAlpha (rt {runtimeProc = q}) ev)
    (ByName p) -> case Map.lookup p (definitions rt) of
        Just definition -> run' (rt {runtimeProc = definition}) ev
        Nothing -> error "Evaluation error: Current process has an undefined process ( " ++ p ++ " )" `seq` (rt, False, False)
    Stop -> (rt, False, False)
    Skip -> (rt, False, False)
