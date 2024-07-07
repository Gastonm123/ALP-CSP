{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      :  CSP.PrettyPrint
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with pretty printing errors, sentences and processes

module PrettyPrint (prettyPrint, Generic(..), errorStyle) where
import AST
import Prettyprinter.Render.Terminal
import Prettyprinter
import Prelude hiding (Left, Right)
import Data.Maybe (fromJust)

errorStyle :: AnsiStyle
errorStyle = color Red <> bold

{- Precedencia:
->              precede a  (4)
| [] |~|        precede a  (3)
/\              precede a  (2)
; ||            precede a  (1)
void            (0)
-}

precedence :: Proc -> Maybe Int
precedence (Prefix _ _) = Just 4
precedence (ExternalChoice _ _) = Just 3
precedence (InternalChoice _ _) = Just 3
precedence (Interrupt _ _) = Just 2
precedence (Sequential _ _) = Just 1
precedence (Parallel _ _) = Just 1
precedence _ = Nothing

prettyPrint :: Generic -> Doc AnsiStyle
prettyPrint (SentG (Assign p q)) = fillSep [pretty p, pretty "=", prettyPrint (ProcG q)]
prettyPrint (SentG (Compare p q)) = fillSep [prettyPrint (ProcG p), pretty "==", prettyPrint (ProcG q)]
prettyPrint (ProcG p) = prettyPrint' p
prettyPrint (Error err) = annotate errorStyle (pretty ("* " ++ err ++ " *"))
prettyPrint _ = error "Not implemented"

prettyPrint' :: Proc -> Doc AnsiStyle
prettyPrint' (ByName p) = pretty p
prettyPrint' Stop = pretty "STOP"
prettyPrint' Skip = pretty "SKIP"
prettyPrint' (Prefix pref q) = let
    precedenceP = fromJust (precedence (Prefix pref q))
  in
    maybe
      (fillSep [pretty pref, pretty "->", prettyPrint' q])
      (\precedenceQ ->
        if precedenceP > precedenceQ
          then fillSep [pretty pref, pretty "->", paren (prettyPrint' q)]
          else fillSep [pretty pref, pretty "->", prettyPrint' q])
      (precedence q)
prettyPrint' p = let 
    (Binary assoc op q r) = fromBinary p
    precedenceP = fromJust (precedence p)
  in fillSep [
      maybe
        (prettyPrint' q)
        (\precedenceQ -> if 
            | precedenceP > precedenceQ -> paren (prettyPrint' q)
            | precedenceP == precedenceQ && assoc == Left -> prettyPrint' q
            | precedenceP < precedenceQ -> prettyPrint' q
            | otherwise -> paren (prettyPrint' q))
        (precedence q),
      pretty op,
      maybe
        (prettyPrint' r)
        (\precedenceR -> if 
            | precedenceP > precedenceR -> paren (prettyPrint' r)
            | precedenceP == precedenceR && assoc == Right -> prettyPrint' r
            | precedenceP < precedenceR -> prettyPrint' r
            | otherwise -> paren (prettyPrint' r))
        (precedence r)
    ]

paren :: Doc AnsiStyle -> Doc AnsiStyle
paren p = pretty "(" <> p <> pretty ")"

data Assoc = Left | Right deriving Eq
data Binary = Binary Assoc String Proc Proc

fromBinary :: Proc -> Binary
fromBinary (InternalChoice p q) = Binary Left "|~|" p q
fromBinary (ExternalChoice p q) = Binary Left "[]" p q
fromBinary (Parallel p q) = Binary Left "||" p q
fromBinary (Sequential p q) = Binary Left ";" p q
fromBinary (Interrupt p q) = Binary Left "/\\" p q
fromBinary _ = error "Incomplete pattern matching in pretty printing"

{-
data MProc = Binary String Proc Proc
           | Prefix Event Proc
           | Atom Proc

class RProc s where
  from :: Proc -> s
  to :: s -> Proc

instance RProc MProc where
  from (InternalChoice p q) = Binary "|~|" p q
  from (ExternalChoice p q) = Binary "[]" p q
  from (Parallel p q) = Binary "||" p q
  from (Sequential p q) = Binary ";" p q
  from (Prefix pref q) = Prefix pref q
  from ()
prettyPrint' :: MProc -> Doc AnsiStyle
prettyPrint' (Binary String Proc)
-}