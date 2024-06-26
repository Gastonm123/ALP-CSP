{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import           CSP.Parser
import           System.Console.GetOpt
import qualified System.Environment            as Env
import           CSP.AST
import           Interactive

import           Prettyprinter -- Doc
import           Prettyprinter.Render.Terminal  (AnsiStyle)
import           CSP.PrettyPrint                (prettyPrint, Generic(..))
import Prelude hiding (error)

---------------------------------------------------------

data Options = Options
  { optPrint :: Bool
  , optAST   :: Bool
  , optEval  :: Int
  , optHelp  :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions =
  Options { optPrint = False, optAST = False, optEval = 0, optHelp = False } 

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
  , Option ['e']
           ["evaluator"]
           (ReqArg (\s opts -> opts { optEval = read s }) "N_EVALUADOR")
           "Elegir evaluador 1, 2 o 3."
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
    case file_parse s of
      Failed error -> print error
      Ok prog -> if
        | optAST opts       -> print prog
        | optPrint opts     -> do
          let hangPrint :: Sentence -> Doc AnsiStyle
              hangPrint p = hang 4 (prettyPrint (SentG p))
          print (vcat (map hangPrint prog))
        | otherwise         -> interactive prog
