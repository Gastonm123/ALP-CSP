{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
-- |
-- Module      :  CSP.PrettyPrint
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with pretty printing errors, sentences and processes

module PrettyPrint where

import Lang
import Prettyprinter
import Prettyprinter.Render.Terminal
import GHC.IO.FD (stdout)

prettyPrint :: Proc -> Doc AnsiStyle
prettyPrint = go
  where
    go (ByName p) = pretty (show p)
    go Stop = pretty "STOP"
    go Skip = pretty "SKIP"
    go (Prefix ev q) =
      case q of
        (InternalChoice _ _) -> prefixBinary
        (ExternalChoice _ _) -> prefixBinary
        (LabeledAlt     _ _) -> prefixBinary
        (Parallel       _ _) -> prefixBinary
        (Sequential     _ _) -> prefixBinary
        (Interrupt      _ _) -> prefixBinary
        _                    -> (fillSep [pretty ev, pretty "->", go q])
      where
        prefixBinary = fillSep [pretty ev, pretty "->", paren (go q)]
      {- Si el proceso q se construye con operadores de menor precedencia
        - entonces parece que la prefijacion se roba la primera parte del 
        - termino. Por ej:
        -    Pref "a" (Parallel (ByName "AB") (ByName "XY"))
        - se renderizaría, sin parentesis
        -    a -> AB || XY
        -}
    go (ExternalChoice p q) = binary  "[]" p q
    go (InternalChoice p q) = binary "|~|" p q
    go (Interrupt      p q) = binary "/\\" p q
    go (LabeledAlt     p q) = binary   "|" p q
    go (Parallel       p q) = binary  "||" p q
    go (Sequential     p q) = binary   ";" p q
    binary op p q = let
      precEnv = precedence op
      envBinary op1 pr =
        if precEnv > precedence op1 then
          paren (go pr)
        else
          go pr
      prettyP = case p of
        (ExternalChoice _ _) -> envBinary  "[]" p
        (InternalChoice _ _) -> envBinary "|~|" p
        (Interrupt      _ _) -> envBinary "/\\" p
        (LabeledAlt     _ _) -> envBinary   "|" p
        (Parallel       _ _) -> envBinary  "||" p
        (Sequential     _ _) -> envBinary   ";" p
        _                    -> go p
      prettyQ = case q of
        (ExternalChoice _ _) -> envBinary  "[]" q
        (InternalChoice _ _) -> envBinary "|~|" q
        (Interrupt      _ _) -> envBinary "/\\" q
        (LabeledAlt     _ _) -> envBinary   "|" q
        (Parallel       _ _) -> envBinary  "||" q
        (Sequential     _ _) -> envBinary   ";" q
        _                    -> go q
      in fillSep [prettyP, pretty op, prettyQ]

{- Precedencia:
->              precede a  (4)
| [] |~|        precede a  (3)
/\              precede a  (2)
; ||            precede a  (1)
-}

precedence :: String -> Int
precedence ( "->") = 4
precedence ( "[]") = 3
precedence ("|~|") = 3
precedence (  "|") = 3
precedence ("/\\") = 2
precedence (  ";") = 1
precedence ( "||") = 1
precedence       _ = error "Error de programacion en prettyprint"

paren :: Doc AnsiStyle -> Doc AnsiStyle
paren p = pretty "(" <> p <> pretty ")"

render :: Doc AnsiStyle -> IO ()
render doc = renderIO stdout ( layoutPretty defaultLayoutOptions doc )
