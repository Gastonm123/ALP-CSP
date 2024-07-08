{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Traces (
  traces
) where

import AST
import Data.Foldable (Foldable(foldl'))
import RunnableProc

expand :: RunnableProc a => [(a, [Event])] -> Event -> [(a, [Event])]
expand threads ev = [(th', if accepted then ev:evs else evs) | (th, evs) <- threads, 
                                                               (th', accepted) <- expandOne th ev]

expandOne :: RunnableProc a => a -> Event -> [(a, Bool)]
expandOne p ev =
  let reject = (p, False)
  in case asProc p of
    (ExternalChoiceRep q r) -> if
      | accept q ev && accept r ev -> expandOne q ev ++ expandOne r ev
      | accept q ev -> expandOne q ev
      | accept r ev -> expandOne r ev
      | otherwise -> [reject]
    (InternalChoiceRep q r) -> if -- internal choice puede descartar un evento aceptado
      | accept q ev && accept r ev -> reject : expandOne q ev ++ expandOne r ev
      | accept q ev -> reject : expandOne q ev
      | accept r ev -> reject : expandOne r ev
      | otherwise -> [reject]
    (PrefixRep _ _) -> if accept p ev then [(run p ev, True)] else [reject]
    (ParallelRep q r) -> let
        expandQ = expandOne q ev
        expandR = expandOne r ev
      in if
      | accept q ev && accept r ev -> [(fromProc (ParallelRep altQ altR), True)
                                        | (altQ, acceptedQ) <- expandQ, 
                                          (altR, acceptedR) <- expandR,
                                          acceptedQ && acceptedR]
      | accept q ev && not (inAlpha r ev) -> [(fromProc (ParallelRep altQ r), True)
                                               | (altQ, acceptedQ) <- expandQ,
                                                 acceptedQ]
      | accept r ev && not (inAlpha q ev) -> [(fromProc (ParallelRep q altR), True)
                                               | (altR, acceptedR) <- expandR,
                                                 acceptedR]
      | otherwise -> [reject]
    (InterruptRep  q r) -> if
      | accept r ev && accept q ev -> [(fromProc (InterruptRep altQ r), True)
                                        | (altQ, acceptedQ) <- expandOne q ev,
                                          acceptedQ]
                                      ++ expandOne r ev
      | accept r ev -> expandOne r ev
      | accept q ev -> [(fromProc (InterruptRep altQ r), True)
                         | (altQ, acceptedQ) <- expandOne q ev,
                           acceptedQ]
      | otherwise -> [reject]
    (SequentialRep q r) ->
      case asProc q of
        StopRep -> [reject]
        SkipRep -> expandOne r ev
        _       -> [(fromProc (SequentialRep altQ r), True)
                     | (altQ, acceptedQ) <- expandOne q ev,
                       acceptedQ]
    (ByNameRep name) -> expandOne (findProc p name) ev
    (StopRep) -> [reject]
    (SkipRep) -> [reject]

traces :: RunnableProc a => a -> [Event] -> [[Event]]
--traces p evs = snd <$> foldl' (\acc ev -> concatMap (flip traces' ev) acc) [(p, [])] evs
traces p evs = snd <$> foldl' expand [(p, [])] evs

