{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Interactive (interactive) where

import CSP.AST
import Control.Monad
import Control.Monad.Except
import Control.Monad.ST
-- Doc

import Data.Char (isLower)
import qualified Data.HashTable.ST.Basic as H
import CSP.Eval
import CSP.PrettyPrint
import Prettyprinter
-- Color

import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO (hFlush, stdout)

interactive :: [Sentence] -> IO ()
interactive prog = do
  defines <- stToIO $ eval prog
  interactive' defines (map SentG prog)

{- Interactive' es recursiva de cola. Para logralo se pasa una computacion
 - stateful de argumento: el estado de la iteracion anterior -}
interactive' :: Namespace RealWorld -> [Generic] -> IO ()
interactive' defines prog = do
  printProgState defines prog
  putStr "CSP> "
  hFlush stdout
  line <- getLine
  case parseLine line of
    (Right events) ->
      let runProg = do
            -- ST s [Either EvalError Proc]
            forM
              prog
              ( \case
                  (SentG (Assign _ q)) -> do
                    let evalRes = evalProcStar defines q
                    runStar evalRes events
                  (ProcG p) -> do
                    let evalRes = evalProcStar defines p
                    runStar evalRes events
                  (Error err) -> return (throwError err)
              )
          format :: ST s [Either EvalError Proc] -> ST s [Generic]
          format = fmap (map (either Error ProcG))
       in do
            prog' <- stToIO $ format runProg
            interactive' defines prog'
    (Left error) -> do
      renderIO
        stdout
        ( layoutPretty
            defaultLayoutOptions
            ( annotate errorStyle (pretty ("!! " ++ error) <> hardline)
            )
        )
      interactive' defines prog

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