{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Interactive (interactive) where

import Control.Monad ( forM )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.ST ( RealWorld, stToIO, ST )
-- Doc

import Data.Char (isLower, isNumber)
import Eval
    ( EvalRandom,
      Prog )
import PrettyPrint ( errorStyle, prettyPrint, Generic(..), render )
import Prettyprinter
    ( Doc,
      annotate,
      hang,
      indent,
      line,
      vcat,
      Pretty(pretty) )

import Prettyprinter.Render.Terminal (AnsiStyle)
import System.IO (hFlush, stdout)
import AST
    ( Proc(Skip),
      Sentence(Eq, NEq, NEqStar, Assign),
      Event )

import RuntimeProc (RuntimeProc(..), initRuntime)
import RunnableProc (RunnableProc(..))
import Compare (compareProcs)
import Control.Monad.Trans.State.Lazy
import System.Random.Stateful (STGen(..))
import qualified Data.Map as Map

{- Traverses the program looking for undefined symbols
 - Arguments:
 -    prog: program
 - Returns:
 -    z: true if there is some symbol undefined
 -}
{-somethingUndefined :: Prog -> Bool
somethingUndefined prog = let
  defines = map (\case
                    (Assign id _) -> id
                    _ -> "") prog

  isDefined  (ByName id) = id `elem` defines
  isDefined  (ExternalChoice p q) = bothDefined p q
  isDefined  (InternalChoice p q) = bothDefined p q
  isDefined  (Parallel p q) = bothDefined p q
  isDefined  (Sequential p q) = bothDefined p q
  isDefined  (Prefix _ p) = isDefined p
  isDefined  (Interrupt p q) = bothDefined p q
  isDefined  (Stop) = True
  isDefined  (Skip) = True
  isDefined' (Assign _ p) = isDefined p
  isDefined' (Eq p q) = bothDefined p q
  isDefined' (NEq p q) = bothDefined p q
  isDefined' (NEqStar p q) = bothDefined p q

  bothDefined p q = isDefined p `par` isDefined q `par` (isDefined p && isDefined q)

  in
    not (all isDefined' prog) -}

{- Arguments:
 -   erandom: random number generator from system entropy or user-provided seed
 -   prog: list of sentences in a CSP file (program)
 - Returns:
 -   io: input-output thread with stateful computations
 -}
interactive :: EvalRandom -> Prog -> IO ()
interactive erandom prog =
  let randomGen = unSTGen erandom
  in
  case initRuntime randomGen prog of
    Just constructRuntime -> let
      rt = (constructRuntime Skip)
      symbols = map (\(k, v) -> SentG $ Assign k v) (Map.toList (definitions rt))
      vcs = map SentG
            (filter (\case
                    Eq _ _ -> True
                    NEq _ _ -> False     -- temporariamente omitido
                    NEqStar _ _ -> False -- temporariamente omitido
                    _ -> False) prog)
      in
      interactive' rt (symbols ++ vcs)
    Nothing -> render
                (annotate errorStyle 
                (pretty ("!! Error: Hay algun simbolo indefinido")
                <> line))

{- Arguments:
 -   defines: hashtable of the program symbols (probably overkill)
 -   erandom: random number generator from system entropy or user-provided seed
 -   prog: list of sentences, errors, or processes which comprise the program
 -         state
 - Returns:
 -   io: input-output thread with stateful computations
 -}
interactive' :: RuntimeProc -> [Generic] -> IO ()
interactive' rt prog = do
  printProgState prog
  putStr "CSP> "
  hFlush stdout
  line <- getLine
  case parseLine line of
    (Right events) ->
      let 
        advanceProg :: State RuntimeProc [Generic]
        advanceProg =
          forM
          prog
          (\generic -> 
            state $ \rt ->
            case generic of
              (SentG (Assign _ q)) ->
                let rtQ = rt {runtimeProc = q}
                    rtQ' = foldl run rtQ events
                in (ProcG (runtimeProc rtQ'), rtQ')
              (SentG (Eq p q)) ->
                let rtP = rt {runtimeProc = p}
                    rtQ = rt {runtimeProc = q}
                    rtP' = foldl run rtQ events
                    rtQ' = foldl run rtQ events
                in
                if (fst $ compareProcs rtP rtQ events)
                  then
                    (SentG
                    (Eq (runtimeProc rtP') (runtimeProc rtQ'))
                    , rtQ')
                  else
                    (Error . show . prettyPrint . SentG $
                    (Eq (runtimeProc rtP') (runtimeProc rtQ'))
                    , rtQ')
              (SentG (NEq p q)) -> undefined
              (SentG (NEqStar p q)) -> undefined
              (ProcG p) ->
                let rtP = rt {runtimeProc = p}
                    rtP' = foldl run rtP events
                in
                (ProcG (runtimeProc rtP'), rtP')
              (Error err) -> (Error err, rt)
          )
        -- ejecutar el for
        (prog', rt') = runState advanceProg rt
       in
        interactive' rt' prog'
    (Left error) -> do
      render
        (annotate errorStyle $
        (pretty ("!! " ++ error))
        <> Prettyprinter.line)
      interactive' rt prog

{- Parse interactive CLI input
 - Arguments:
 -    s: interactive CLI input
 - Returns:
 -    e: either a list of user-provided events or an error
 -}
parseLine :: String -> Either String [Event]
parseLine s = do
  evs <- parseLine' s []
  return (reverse evs)

{- Parse interactive CLI input
 - Arguments:
 -    s: interactive CLI input
 -    acc: accumulated events
 - Returns:
 -    e: either a list of user-provided events or an error
 -}
parseLine' :: String -> [Event] -> Either String [Event]
parseLine' s acc = case s of
  [] -> return acc
  (' ' : cs) -> parseLine' cs acc
  (c : cs)
    | isNumber c ->
        case span isNumber (c : cs) of
          (index, '.':rest)
            | isLower (head rest) ->
              let (name, line) = span
                                 (allowedChars [isLower, isNumber, (==) '_', (==) '.'])
                                 rest
              in parseLine' line ((index++"."++name):acc)
            | otherwise -> throwError ("Error de escritura en el simbolo " ++ [head rest])
          _ -> throwError ("Error de escritura en el simbolo " ++ [c])
    | isLower c ->
        let (event, rest) = span
                            (allowedChars [isLower, isNumber, (==) '_', (==) '.'])
                            (c:cs)
        in parseLine' rest (event : acc)
    | otherwise -> throwError ("Error de escritura en el simbolo " ++ [c])

allowedChars :: [Char -> Bool] -> Char -> Bool
allowedChars allowed x = any (\f -> f x) allowed

{- Print all sentences, processes and errors in the program state
 - Arguments:
 -    prog: program
 - Returns:
 -    io: input-output action that prints the program state
 -}
printProgState :: [Generic] -> IO ()
printProgState prog =
  let progState :: ST RealWorld (Prettyprinter.Doc AnsiStyle)
      progState = do
        let hangPrint gen = Prettyprinter.hang 4 (prettyPrint gen)
        return (Prettyprinter.vcat (map hangPrint prog))
   in do
        doc <- stToIO progState
        render (Prettyprinter.indent 4 doc <> Prettyprinter.line)