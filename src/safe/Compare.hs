{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compare (
  traces,
  Trace(..),
  generateRandomEvents,
  compareProcs,
  verifyEqual,
  verifyNEqual,
  verifyNEqualStar,
  TestDescription(..)
) where

import RunnableProc
import AST

import qualified Data.Sequence as Seq
import Control.Monad.State.Strict (evalState, MonadState (state), runState)
import Control.Monad (replicateM)
import Prettyprinter.Internal (Pretty(..), align, vsep, (<+>), annotate)
import qualified Data.Set as Set
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter (Doc, line, emptyDoc)
import Control.Monad.Trans.Accum (runAccum)
import Control.Monad.Accum (MonadAccum (add))
import Data.Maybe
import Data.List
import PrettyPrint (errorStyle)
import Data.Foldable (foldlM)

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
            {-
            internalChoice (altQ, acceptQ) (altR, acceptR) = -- no determinismo angelico: elegimos el escenario mas positivo cuando
                                                             -- podriamos rechazar el evento en todos los casos salvo uno
              if
              | acceptQ && acceptR -> (fromProc (InternalChoiceRep altQ altR), True)
              | acceptQ -> (altR, acceptR)
              | acceptR -> (altQ, acceptQ)
              | otherwise -> reject
            -}
          in
            -- [internalChoice impQ impR | impQ <- expandQ, impR <- expandR] -- iteramos por todas las "implementaciones" de P/ev y Q/ev
            --                                                               -- idea de R. Hoare
            expandQ ++ expandR -- no usamos "nodeterminismo angelico" ni "nodeterminismo demonico"
        (LabeledAltRep q r) -> let
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


class TestDescription a where
  repetitions :: a -> Int
  testLength :: a -> Int
  rng :: a -> [Int]
  splitRng :: a -> ([Int], a) -- generate an independent random number list

generateRandomEvents :: (RunnableProc a, TestDescription d) => a -> d -> [[Event]]
generateRandomEvents rt desc = let
  alpha = Seq.fromList (getAlpha rt) :: Seq.Seq Event
  lenAlpha = Data.List.length alpha
  indices =
    evalState
    (replicateM (repetitions desc)
    (state (splitAt (lenAlpha * (testLength desc)))))
    (rng desc)
  in (map (\i -> (alpha `Seq.index` (i `mod` lenAlpha))) <$> indices)

compareProcs :: (RunnableProc a) => a -> a -> [Event] -> (Bool, Doc AnsiStyle)
compareProcs rtA rtB evs = runAccum compareM emptyDoc
  where
    compareM =
      let
        alphaA = Set.fromList (getAlpha rtA)
        alphaB = Set.fromList (getAlpha rtB)
        tracesA = traces rtA evs
        tracesB = traces rtB evs
        trieA = mkTrie tracesA :: TraceTrie
        trieB = mkTrie tracesB :: TraceTrie
      in if (alphaA == alphaB)
            && all (inTrie trieA) tracesB
      then return True
      else do
        add (annotate errorStyle
                  (pretty "Una comparacion ha fallado\n"
                  <>pretty "    "
                  <>pretty (showProc rtA) <+> pretty "==" <+> pretty (showProc rtB)
                  <>line))
        if alphaA /= alphaB then do
          add (pretty "Alfabeto del lado izquierdo: "
                    <> pretty (commaSeparated alphaA)
                    <> line)
          add (pretty "Alfabeto del lado derecho: "
                    <> pretty (commaSeparated alphaB)
                    <> line)
        else do
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
          add (pretty "Caso de prueba:\n"
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
                              [evs]))
        return False
    commaSeparated :: Foldable t => t String -> String
    commaSeparated = foldr
                     (\x acc -> if acc /= "" then x <> ", " <> acc else x)
                     ""

verifyEqual :: (RunnableProc a, TestDescription d) => a -> a -> d -> (Bool, Doc AnsiStyle)
verifyEqual rtA rtB d = runAccum compareM emptyDoc
  where
    compareM =
      let
        alphaA = Set.fromList (getAlpha rtA)
        alphaB = Set.fromList (getAlpha rtB)
        tests = generateRandomEvents rtA d
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
                  <>pretty (showProc rtA) <+> pretty "==" <+> pretty (showProc rtB)
                  <>line)
        if alphaA /= alphaB then do
          add (pretty "Alfabeto del lado izquierdo: "
                    <> pretty (commaSeparated alphaA)
                    <> line)
          add (pretty "Alfabeto del lado derecho: "
                    <> pretty (commaSeparated alphaB)
                    <> line)
        else do
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

verifyNEqual :: (RunnableProc a, TestDescription p) => a -> a -> p -> (Bool, Doc AnsiStyle)
verifyNEqual rtP rtQ td =
  let (eq, _) = verifyEqual rtP rtQ td
  in
    (not eq,
      if eq
      then pretty "No se pudo verificar que"
      <+> pretty (showProc rtP) <+> pretty "y"
      <+> pretty (showProc rtQ) <+> pretty "son distintos"
      <> line
      else pretty "" <> line)

verifyNEqualStar :: (RunnableProc a, TestDescription p) => a -> a -> p -> (Bool, Doc AnsiStyle)
verifyNEqualStar rtP rtQ td =
  let
    (randomNums, td') = splitRng td
    alpha = Seq.fromList (getAlpha rtP) Seq.>< Seq.fromList (getAlpha rtQ)
    lenAlpha = Seq.length alpha
    indices =
      evalState
      (replicateM (repetitions td)
      (state (splitAt (lenAlpha * (testLength td)))))
      randomNums
    evss = (map (\i -> (alpha `Seq.index` (i `mod` lenAlpha))) <$> indices)
    notEqStar =
      all (fst . (\evs ->
          runState
            (foldlM
              (\notEq ev ->
                state $ \(rtP', rtQ') ->
                  (notEq || (not (fst (verifyEqual rtP' rtQ' td')))
                  , (run rtP' ev, run rtQ' ev)))
              False
              evs)
            (rtP, rtQ))) evss
  in
    (notEqStar,
      if notEqStar
      then pretty "" <> line
      else pretty "No se pudo verificar que"
      <+> pretty (showProc rtP) <+> pretty "y"
      <+> pretty (showProc rtQ) <+> pretty "son siempre distintos"
      <> line)