{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           ParserMonad
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           Lang
import           Parser

import           Prettyprinter -- Doc
import           Prettyprinter.Render.Terminal  (AnsiStyle)
-- import           PrettyPrint                    (prettyPrint, render, errorStyle, successStyle)

import           Prelude hiding (error)
import           Data.List (find)
import           Data.Maybe
import           GHC.Base (error)
---------------------------------------------------------

data Options = Options
  { optPrint :: Bool
  , optAST   :: Bool
  , optHelp  :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options {
    optPrint = False
    , optAST = False
    , optHelp = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['p']
           ["print"]
           (NoArg (\opts -> opts { optPrint = True }))
           "Imprimir el programa de entrada."
  , Option ['a']
           ["AST"]
           (NoArg (\opts -> opts { optAST = True }))
           "Mostrar el AST del programa de entrada."
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "Imprimir guia de uso."
  ]

finalOptions :: [String] -> IO (Options, [String])
finalOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Uso:"

main :: IO ()
main = do
  s : opts   <- Env.getArgs
  (opts', _) <- finalOptions opts
  runOptions s opts'

runOptions :: FilePath -> Options -> IO ()
runOptions fp opts
  | optHelp opts = putStrLn (usageInfo "Uso: " options)
  | otherwise = do
    s <- readFile fp
    case parseFile s 0 of
      Failed error -> print (pretty error)
      Ok prog -> if
        | optAST opts       -> print prog
        | optPrint opts     -> do
            let hangPrint :: Sentence -> Doc AnsiStyle
                hangPrint p = hang 4 (prettyPrint (SentG p))
            print (vcat (map hangPrint prog))
        | otherwise -> error "optHelp ya fue matcheado"