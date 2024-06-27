{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interactive (interactive) where

import AST
import Control.Monad
import Control.Monad.Except
import Control.Monad.ST
-- Doc

import Data.Char (isLower)
import qualified Data.HashTable.ST.Basic as H
import Eval
import PrettyPrint
import Prettyprinter
-- Color

import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO (hFlush, stdout)
import Control.Parallel (par)

somethingUndefined :: [Sentence] -> Bool
somethingUndefined prog = let
  defines = map (\(Assign id _) -> id) prog

  isDefined  (ByName id) = id `elem` defines
  isDefined  (ExternalChoice p q) = bothDefined p q
  isDefined  (InternalChoice p q) = bothDefined p q
  isDefined  (Parallel p q) = bothDefined p q
  isDefined  (Sequential p q) = bothDefined p q
  isDefined  (Prefix _ p) = isDefined p
  isDefined  (Interrupt p q) = bothDefined p q
  isDefined  (Stop) = True
  isDefined  (Skip) = True
  isDefined  _ = error "undefined"
  isDefined' (Assign _ p) = isDefined p

  bothDefined p q = isDefined p `par` isDefined q `par` (isDefined p || isDefined q)

  in
    not (all isDefined' prog)

{- Arguments:
 -   erandom: random number generator from system entropy or user-provided seed
 -   prog: list of sentences in a CSP file
 - Returns:
 -   io: input-output thread with stateful computations
 -}
interactive :: EvalRandom RealWorld -> [Sentence] -> IO ()
interactive erandom prog = 
  if somethingUndefined prog
  then renderIO
      stdout
      ( layoutPretty
          defaultLayoutOptions
          ( annotate errorStyle (pretty ("!! Error: Hay algun simbolo indefinido") <> hardline)
          )
      )
  else do
    defines <- stToIO $ eval prog
    interactive' defines erandom (map SentG prog)

{- Arguments:
 -   defines: hashtable of the program symbols (probably overkill)
 -   erandom: random number generator from system entropy or user-provided seed
 -   prog: list of sentences, errors, or processes which comprise the program
 -         state
 - Returns:
 -   io: input-output thread with stateful computations
 -}
interactive' :: Namespace RealWorld -> EvalRandom RealWorld -> [Generic] -> IO ()
interactive' defines erandom prog = do
  printProgState defines prog
  putStr "CSP> "
  hFlush stdout
  line <- getLine
  case parseLine line of
    (Right events) ->
      let runProg :: IO [Generic]
          runProg = do
            forM
              prog
              ( \case
                  (SentG (Assign _ q)) -> do
                    let evalRes = evalProcStar defines erandom q
                    q1 <- stToIO $ runStar evalRes events
                    return (ProcG q1)
                  (ProcG p) -> do
                    let evalRes = evalProcStar defines erandom p
                    q1 <- stToIO $ runStar evalRes events
                    return (ProcG q1)
                  (Error err) -> return (Error err)
              )
       in do
            prog' <- runProg
            interactive' defines erandom prog'
    (Left error) -> do
      renderIO
        stdout
        ( layoutPretty
            defaultLayoutOptions
            ( annotate errorStyle (pretty ("!! " ++ error) <> hardline)
            )
        )
      interactive' defines erandom prog

parseLine :: String -> Either String [Event]
parseLine s = parseLine' s []

parseLine' :: String -> [Event] -> Either String [Event]
parseLine' s acc = case s of
  [] -> return acc
  (' ' : cs) -> parseLine' cs acc
  (c : cs)
    | isLower c ->
        let (event, rest) = span isLower (c : cs)
         in parseLine' rest (event : acc)
    | True -> throwError ("Error de escritura en el simbolo " ++ [c])

printProgState :: Namespace RealWorld -> [Generic] -> IO ()
printProgState defines prog =
  let progState :: ST RealWorld (Doc AnsiStyle)
      progState = do
        prog' <- mapM (replaceByDef defines) prog
        let hangPrint gen = hang 4 (prettyPrint gen)
        return (vcat (map hangPrint prog'))
   in do
        doc <- stToIO progState
        renderIO
          stdout
          ( layoutPretty
              defaultLayoutOptions
              (indent 4 doc <> line)
          )

replaceByDef :: Namespace RealWorld -> Generic -> ST RealWorld Generic
replaceByDef defines (ProcG (ByName p)) =
  maybe (Error (p ++ " Simbolo no definido")) ProcG
    <$> H.lookup defines p
replaceByDef _ generic = return generic