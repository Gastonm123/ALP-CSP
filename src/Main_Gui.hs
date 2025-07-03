{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main_Gui (main) where

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

import Gui
---------------------------------------------------------

main :: IO ()
main = return startGui