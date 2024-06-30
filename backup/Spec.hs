import Eval
import AST
import Parser
import Control.Monad.ST
-- import Data.Strict.Maybe
import qualified Data.HashTable.ST.Basic as H

-- <&> es lo mismo que >>= return . eval . (\(Ok a) -> [a]) . line_parse
main = getLine <&> eval . (\(Ok a) -> [a]) . line_parse

fromJust (Just x) = x
fromJust Nothing = error "Error: fromJust received Nothing"

fromRight (Right x) = x
fromRight (Left x) = error "Error: fromRight received Left"

{- "gas" y "lucas" nunca van a ser aceptados por la maquina -}
main' = do
  -- line <- getLine
  defines  <- (stToIO . eval . (\(Ok a) -> [a]) . line_parse)
              "GAS = pepe -> GAS [] gas -> GAS [] lucas -> GAS ||| pepe -> GAS"
  evalResult  <- stToIO (H.lookup defines "GAS")
  evalResult' <- stToIO (evalProc defines $ fromJust evalResult)
  print $ run evalResult' "pepe"
  print $ run evalResult' "lucas"
  print $ run evalResult' "gas"
  print $ refusal evalResult' "pepe"
  -- Veamos que pasa si ejecutamos algo mas luego de "pepe"
  evalResult'' <- stToIO (evalProc defines $ fromRight (run evalResult' "pepe"))
  print $ run evalResult'' "pepe"
  print $ run evalResult'' "lucas"
  print $ run evalResult'' "gas"
  print $ refusal evalResult' "gas"

main'' = do
  defines <- (stToIO . eval . (\(Ok a) -> [a]) . line_parse)
               "A = on -> off -> A"
  evalResult  <- stToIO (H.lookup defines "A")
  let evalStarResult = evalProcStar defines $ fromJust evalResult
  runResult <- stToIO (runStar evalStarResult ["on", "off", "pepe", "on", "on", "off", "on"])
  refusalResult <- stToIO (refusalStar evalStarResult ["on", "off", "pepe", "on", "on", "off", "on"])
  print runResult
  print refusalResult
  
main''' = do
  defines <- (stToIO . eval . (\(Ok a) -> [a]) . line_parse)
              "GAS = pepe -> GAS [] gas -> GAS [] lucas -> GAS ||| pepe -> GAS"
  evalResult  <- stToIO (H.lookup defines "GAS")
  let evalStarResult = evalProcStar defines $ fromJust evalResult
  runResult <- stToIO (runStar evalStarResult ["pepe", "pepe", "pepe", "pepe"])
  print runResult