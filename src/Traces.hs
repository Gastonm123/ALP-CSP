{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Traces (
  traces,
  Trace(..),
  TraceTrie(..),
  Trie(..),
  generateEvents
) where

import RunnableProc
import AST

import Data.Foldable (Foldable(foldl'))
import qualified Data.Sequence as Seq
import System.Random (RandomGen, Random (randoms))
import Control.Monad.State.Strict (evalState, MonadState (state))
import Control.Monad (replicateM)

newtype Trace = Trace { unTrace :: [Event] } deriving (Show, Eq)

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

traces :: RunnableProc a => a -> [Event] -> [Trace]
--traces p evs = snd <$> foldl' (\acc ev -> concatMap (flip traces' ev) acc) [(p, [])] evs
traces p evs = Trace . snd <$> foldl' expand [(p, [])] evs

data TraceTrie = Internal [(Event, TraceTrie)] | Leaf deriving Show

class Trie a d where
  mkTrie :: (Foldable t) => t d -> a
  inTrie :: a -> d -> Bool

instance Trie TraceTrie Trace where
  mkTrie :: (Foldable t) => t Trace -> TraceTrie
  mkTrie evss = foldl' (\trie (Trace evs) -> extendTrie trie evs) Leaf evss
    where
      extendTrie (Internal children) evs = Internal (extendChildren children evs)
      extendTrie Leaf (ev:evs) = Internal [(ev, extendTrie Leaf evs)]
      extendTrie Leaf [] = Leaf
      extendChildren (child:children) (ev:evs) = if (fst child) == ev
                                              then (ev, extendTrie (snd child) evs) : children
                                              else child : extendChildren children (ev:evs)
      extendChildren []               (ev:evs) = [(ev, extendTrie Leaf evs)]
      extendChildren children         [] = children

  inTrie :: TraceTrie -> Trace -> Bool
  inTrie trie (Trace evs) = inTrie' trie evs
    where
      inTrie' (Internal children) (ev:evs) =  case lookup ev children of
        Just next -> inTrie' next evs
        Nothing -> False
      inTrie' (Internal _) [] = True
      inTrie' Leaf evs = not (null evs)

data TestDuration = TestDuration { numTest :: Int, multiplier :: Int }
defaultTest :: TestDuration
defaultTest = TestDuration { numTest = 1000, multiplier = 20 }

generateEvents :: (RunnableProc a, RandomGen g) => a -> g -> [[Event]]
generateEvents rt randomGen = let
  alpha = Seq.fromList (getAlpha rt) :: Seq.Seq Event
  lenAlpha = length alpha
  indices = 
    evalState
      (replicateM (numTest defaultTest)
      (state (splitAt (lenAlpha * multiplier defaultTest))))
    (randoms randomGen)
  in (map (\i -> 
      (alpha `Seq.index` (i `mod` lenAlpha))
    ) <$> indices)
