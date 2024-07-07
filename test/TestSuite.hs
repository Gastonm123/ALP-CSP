{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main (main) where

import Utils ( runProgWithEvents' )
import Parser ( file_parse, ParseResult(Failed, Ok) )
import Eval ( Prog, Namespace, alpha, eval )
import AST ( Event, Generic(Error), Sentence(Assign, Compare) )

import Control.Monad (forM, replicateM)
import Control.Monad.State.Strict (state, evalState)
import System.Random (randoms, mkStdGen)
import Control.Monad.ST (stToIO, ST)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import qualified Data.Set as Set
import Prettyprinter ( (<+>), indent, Pretty(pretty), hsep, vsep )
import PrettyPrint (prettyPrint)

data Test = Test { name :: String, file :: String }

tests :: [Test]
tests = [
  Test { name = "Leyes1", file = "examples/Leyes1.csp" },
  Test { name = "Leyes2", file = "examples/Leyes2.csp" },
  Test { name = "Leyes3", file = "examples/Leyes3.csp" }]

generateEvents :: Namespace s -> Prog -> ST s [[Event]]
generateEvents ns prog = do
  alphas <- forM prog (\case
    (Compare p q) -> (++) <$> (alpha ns p) <*> (alpha ns q)
    (Assign _ p) -> (alpha ns p))
  let alphaProg = Set.toList (Set.fromList (concat alphas))
  let lenAlpha = length alphaProg
  let {
    indices = 
      evalState 
        (replicateM 1000
        (state (splitAt 10)))
      (randoms (mkStdGen 2024))
  }
  return ((map (\i -> alphaProg !! (i `mod` lenAlpha))) <$> indices)

isError :: Generic -> Bool
isError (Error _) = True
isError _ = False

failure :: [[Generic]] -> Bool
failure = any (any isError)

runTests :: IO [Bool]
runTests = do
  progs' <- forM tests 
    (\t -> do
      parse <- file_parse <$> readFile (file t)
      case parse of
        (Ok p) -> do
          print $ 
            pretty "[TestSuite] Archivo cargado"
            <+> pretty (file t)
          return [p]
        (Failed parseError) -> do
          print $
            pretty "[TestSuite] Error cargando"
            <+> pretty (file t)
            <> pretty "\n"
            <> pretty "    Error:"
            <+> pretty parseError
          return [])
  let progs = concat progs'
  
  forM (zip tests progs) (\(t, prog) -> do
    print ((pretty "[TestSuite] Corriendo") <+> (pretty (name t)))
    ns     <- stToIO (eval prog)
    evs    <- stToIO (generateEvents ns prog)
    result <- (stToIO (forM evs (runProgWithEvents' ns prog))) :: IO [[Generic]]
    if failure result
    then do
      let failureIndex = (fromJust (findIndex (any isError) result)) :: Int
      let failureCase  = evs !! failureIndex
      print (
        (pretty "[TestSuite] El test")
        <+> (pretty (name t))
        <+> (pretty "ha fallado"))
      print (
        indent 4 (
        (pretty "Detalles\n")
        <> (pretty "Traza:")
        <+> (pretty failureCase)))
      return False
    else do
      print (
        (pretty "[TestSuite] El test")
        <+> (pretty (name t))
        <+> (pretty "corrio exitosamente"))
      return True)

main :: IO Bool
main =
  and <$> runTests -- Did no test fail