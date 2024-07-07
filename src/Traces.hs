{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Traces where
import AST
import Control.Monad.ST
import Data.Foldable (Foldable(foldl'))
import RunnableProc

expand :: RunnableProc a => [a] -> Event -> [(a, Bool)]
expand ps ev = concatMap ps (flip expandOne ev)

expandOne :: RunnableProc a => a -> Event -> [(a, Bool)]
expandOne p ev = do
  case asProc p of
    (ExternalChoiceRep q r) -> if
      | accept q ev && accept r ev -> expandOne q ev ++ expandOne r ev
      | accept q ev -> expandOne q ev
      | accept r ev -> expandOne r ev
      | otherwise -> [(p, False)]
    (InternalChoiceRep q r) -> if
      | accept q ev && accept r ev -> expandOne q ev ++ expandOne r ev
      | accept q ev -> expandOne q ev
      | accept r ev -> expandOne r ev
      | otherwise -> [(p, False)]
    (PrefixRep _ _) -> if accept p ev then [(run p ev, True)] else [(p, False)]
    (ParallelRep q r) -> let
        expandQ = expandOne q ev
        expandR = expandOne r ev
      in if
      | accept q ev && accept r ev -> [Parallel altQ altR | altQ <- expandQ, altR <- expandR]
      | accept q ev && not (inAlpha r ev) -> [Parallel altQ r | altQ <- expandQ]
      | accept r ev && not (inAlpha q ev) -> [Parallel q altR | altR <- expandR]
      | otherwise -> [p]
    (InterruptRep  q r) -> if
      | accept r ev && accept q ev -> [Interrupt altQ r | altQ <- expandOne q ev]
                                      ++ expandOne r ev
      | accept r ev -> expandOne r ev
      | accept q ev -> [Interrupt altQ r | altQ <- expandOne q ev]
      | otherwise -> [p]
    (SequentialRep q r) ->
      case q of
        Stop -> []
        Skip -> expandOne r ev
        _    -> [Sequential altQ r | altQ <- expandOne q ev]
    (ByNameRep name) -> expandOne (findProc p name) ev
    (StopRep) -> []
    (SkipRep) -> []
      
traces :: RunnableProc a => a -> [Event] -> [[Event]]
traces p evs = snd <$> foldl' (\ev acc -> map (flip traces' ev) acc) [(p, [])] evs

traces' :: RunnableProc a => ()
traces' (p, traceP) ev = [(altP, if accepted then ev:traceP else traceP)
                          | (altP, accepted) <- expand p ev]
