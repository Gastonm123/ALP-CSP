{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           ParserMonad
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           Parser
import           Lang
import           PrettyPrint
import           Elab

import           Prettyprinter -- Doc

import           Prelude hiding (error)
import           Run
import Control.Monad.Reader (ReaderT(runReaderT))
---------------------------------------------------------

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
    case parseFile s 1 of
      Failed err -> print (pretty err)
      Ok prog -> let 
        eprog = elabProg prog
        config = if optDebug opts then
            defaultConfiguration { debug = True }
          else
            defaultConfiguration
        in if
          | optAST opts -> 
            render (vsep (map (pretty . show) (sentences eprog)))
          | optPrint opts -> (render . prettyPrint) eprog
          | otherwise -> runReaderT (run eprog) config


data Options = Options
  { optPrint :: Bool
  , optAST   :: Bool
  , optHelp  :: Bool
  , optDebug :: Bool
  }
  deriving Show

finalOptions :: [String] -> IO (Options, [String])
finalOptions argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Uso:"

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
  , Option ['d']
           ["debug"]
           (NoArg (\opts -> opts { optDebug = True }))
           "Mostrar informacion de debugging"
  ]

defaultOptions :: Options
defaultOptions =
  Options {
    optPrint = False
    , optAST = False
    , optHelp = False
    , optDebug = False
  }

defaultConfiguration :: Conf
defaultConfiguration =
  Conf {
    debug = False
  }