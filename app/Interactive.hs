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
    ( eval,
      evalProcStar,
      EvalRandom,
      EvalStarResult(runStar, trace),
      Namespace,
      Prog )
import PrettyPrint ( errorStyle, prettyPrint, Generic(..) )
import Prettyprinter
    ( Doc,
      annotate,
      defaultLayoutOptions,
      hang,
      hardline,
      indent,
      layoutPretty,
      line,
      vcat,
      Pretty(pretty) )

import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO (hFlush, stdout)
import Control.Parallel (par)
import AST
    ( Proc(Skip, ByName, ExternalChoice, InternalChoice, Parallel,
           Sequential, Prefix, Interrupt, Stop),
      Sentence(Compare, Assign),
      Event )

{- Traverses the program looking for undefined symbols
 - Arguments:
 -    prog: program
 - Returns:
 -    z: true if there is some symbol undefined
 -}
somethingUndefined :: Prog -> Bool
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
  isDefined  _ = error "undefined"
  isDefined' (Assign _ p) = isDefined p
  isDefined' (Compare p q) = bothDefined p q

  bothDefined p q = isDefined p `par` isDefined q `par` (isDefined p || isDefined q)

  in
    not (all isDefined' prog)

{- Arguments:
 -   erandom: random number generator from system entropy or user-provided seed
 -   prog: list of sentences in a CSP file (program)
 - Returns:
 -   io: input-output thread with stateful computations
 -}
interactive :: EvalRandom RealWorld -> Prog -> IO ()
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
  printProgState prog
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
                  (SentG (Compare p q)) -> do
                    let evalP = evalProcStar defines erandom p
                    let evalQ = evalProcStar defines erandom q
                    traceP <- stToIO $ trace evalP events 
                    traceQ <- stToIO $ trace evalQ events
                    if (traceP /= traceQ)
                      then return (Error (show (prettyPrint (SentG (Compare p q)))))
                      else do
                        p1 <- stToIO $ runStar evalP events
                        q1 <- stToIO $ runStar evalQ events
                        return (SentG (Compare p1 q1))
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

{- Parse interactive CLI input
 - Arguments:
 -    s: interactive CLI input
 - Returns:
 -    e: either a list of user-provided events or an error
 -}
parseLine :: String -> Either String [Event]
parseLine s = parseLine' s []

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
    | isLower c ->
        let (event, rest) = span ((||) <$> isLower <*> isNumber) (c : cs)
         in parseLine' rest (event : acc)
    | True -> throwError ("Error de escritura en el simbolo " ++ [c])

{- Print all sentences, processes and errors in the program state
 - Arguments:
 -    prog: program
 - Returns:
 -    io: input-output action that prints the program state
 -}
printProgState :: [Generic] -> IO ()
printProgState prog =
  let progState :: ST RealWorld (Doc AnsiStyle)
      progState = do
        -- prog' <- mapM (replaceByDef defines) prog
        let hangPrint gen = hang 4 (prettyPrint gen)
        return (vcat (map hangPrint prog))
   in do
        doc <- stToIO progState
        renderIO
          stdout
          ( layoutPretty
              defaultLayoutOptions
              (indent 4 doc <> line)
          )

{-
replaceByDef :: Namespace RealWorld -> Generic -> ST RealWorld Generic
replaceByDef defines (ProcG (ByName p)) =
  maybe (Error (p ++ " Simbolo no definido")) ProcG
    <$> H.lookup defines p
replaceByDef _ generic = return generic
-}