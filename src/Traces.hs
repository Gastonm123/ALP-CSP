{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Traces (
  traces,
  Trace(..),
  TraceTrie(..),
  Trie(..),
  generateRandomEvents
) where

import RunnableProc
import AST

import Data.Foldable (Foldable(foldl'))
import qualified Data.Sequence as Seq
import System.Random (RandomGen, Random (randoms))
import Control.Monad.State.Strict (evalState, MonadState (state))
import Control.Monad (replicateM)
import Prettyprinter.Internal (Pretty(..), align, vsep, (<+>))

newtype Trace = Trace { unTrace :: [Event] } deriving (Show, Eq)

expand :: RunnableProc a => [(a, [Event])] -> Event -> [(a, [Event])]
expand threads ev = [(th', if accepted then ev:evs else evs) | (th, evs) <- threads,
                                                               (th', accepted) <- expandOne th ev]

expandOne :: RunnableProc a => a -> Event -> [(a, Bool)]
expandOne p_ ev = expand_ p_
  where
  expand_ p =
    let reject = (p, False)
    in case asProc p of
        (ExternalChoiceRep q r) -> let
            expandQ = expand_ q
            expandR = expand_ r
            externalChoice (altQ, acceptQ) (altR, acceptR) =
              if
              | acceptQ && acceptR -> (fromProc (ExternalChoiceRep altQ altR), True)
              | acceptR -> (altR, acceptR)
              | acceptQ -> (altQ, acceptQ)
              | otherwise -> reject
          in
            [externalChoice impQ impR | impQ <- expandQ, impR <- expandR] -- iteramos por todas las "implementaciones" de P/ev y Q/ev
                                                                          -- idea de R. Hoare
        (InternalChoiceRep q r) -> let
            expandQ = expand_ q
            expandR = expand_ r
            internalChoice (altQ, acceptQ) (altR, acceptR) = -- no determinismo angelico: elegimos el escenario mas positivo cuando
                                                             -- podriamos rechazar el evento en todos los casos salvo uno
              if
              | acceptQ && acceptR -> (fromProc (InternalChoiceRep altQ altR), True)
              | acceptQ -> (altR, acceptR)
              | acceptR -> (altQ, acceptQ)
              | otherwise -> reject
          in
            [internalChoice impQ impR | impQ <- expandQ, impR <- expandR] -- iteramos por todas las "implementaciones" de P/ev y Q/ev
                                                                          -- idea de R. Hoare
            ++ expandQ ++ expandR -- las implementaciones de P |~| Q incluyen el "nodeterminismo angelico", P o Q
        (PrefixRep pref q) -> if ev == pref then [(q, True)] else [reject]
        (ParallelRep q r) -> let
            expandQ = expand_ q
            expandR = expand_ r
            inAlphaR = inAlpha r ev
            inAlphaQ = inAlpha q ev
            parallel (altQ, acceptQ) (altR, acceptR) =
              if
              | acceptQ && acceptR -> (fromProc  (ParallelRep altQ altR), True)
              | acceptQ && not inAlphaR -> (fromProc (ParallelRep altQ r), True)
              | acceptR && not inAlphaQ -> (fromProc (ParallelRep q altR), True)
              | otherwise -> reject
          in
            [parallel impQ impR | impQ <- expandQ, impR <- expandR] -- iteramos por todas las "implementaciones" de P/ev y  Q/ev
                                                                    -- idea de R. Hoare
        (InterruptRep  q r) -> let
            expandQ = expand_ q
            expandR = expand_ r
            interrupt (altQ, acceptQ) (altR, acceptR) = 
              if
              | acceptQ && acceptR -> (fromProc 
                                      (InternalChoiceRep 
                                        (fromProc (InterruptRep altQ r)) altR)
                                      , True)
              | acceptQ -> (fromProc (InterruptRep altQ r), acceptQ)
              | acceptR -> (altR, acceptR)
              | otherwise -> reject
          in
            [interrupt impQ impR | impQ <- expandQ, impR <- expandR] -- iteramos por todas las "implementaciones" de P/ev y Q/ev
                                                                     -- idea de R. Hoare
        (SequentialRep q r) ->
          case asProc q of
            StopRep -> [reject]
            SkipRep -> expand_ r
            _       -> [(fromProc (SequentialRep altQ r), acceptedQ)
                        | (altQ, acceptedQ) <- expand_ q]
        (ByNameRep name) -> expand_ (findProc p name)
        (StopRep) -> [reject]
        (SkipRep) -> [reject]

traces :: RunnableProc a => a -> [Event] -> [Trace]
--traces p evs = snd <$> foldl' (\acc ev -> concatMap (flip traces' ev) acc) [(p, [])] evs
traces p evs = (Trace . reverse . snd <$> foldl' expand [(p, [])] evs)

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
  inTrie trie (Trace evs_) = inTrie' trie evs_
    where
      inTrie' (Internal children) (ev:evs) = case lookup ev children of
        Just next -> inTrie' next evs
        Nothing -> False
      inTrie' (Internal _) [] = True
      inTrie' Leaf evs = null evs

instance Pretty TraceTrie where
  pretty (Internal children) = --pretty "Internal"
                                align
                                (vsep (map
                                      (\(ev, child) -> case child of
                                          Leaf -> pretty ev
                                          _    -> pretty ev
                                              <+> pretty "->"
                                              <+> pretty child)
                                      children))
  pretty Leaf = -- pretty "Leaf"
                pretty ""


data TestDuration = TestDuration { numTest :: Int, multiplier :: Int }
defaultTest :: TestDuration
defaultTest = TestDuration { numTest = 10, multiplier = 5 }

generateRandomEvents :: (RunnableProc a, RandomGen g) => a -> g -> [[Event]]
generateRandomEvents rt randomGen = let
  alpha = Seq.fromList (getAlpha rt) :: Seq.Seq Event
  lenAlpha = length alpha
  indices =
    evalState
    (replicateM (numTest defaultTest)
    (state (splitAt (lenAlpha * multiplier defaultTest))))
    (randoms randomGen)
  in (map (\i -> (alpha `Seq.index` (i `mod` lenAlpha))) <$> indices)
