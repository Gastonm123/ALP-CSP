{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lexer (lexer, Token(..)) where

import Lang ()
import ParserMonad ( P, failPos, ParseResult, failPos' )
import Data.Char ( isNumber, isAlpha, isSpace, isLower, isUpper )
import Debug.Trace (trace)

lexer :: (Token -> P a) -> P a
lexer cont s = {-trace (take 10 s) $-} case s of
    [] -> cont TokenEOF []
    ('\n':cs) -> \line -> lexer cont cs (line + 1)
    (c:cs)
        | isSpace c -> {-trace "1" $-} lexer cont cs
        | isAlpha c -> {-trace "2" $-} lexWord (c:cs)
        | isNumber c -> {-trace "3" $-} lexNumber (c:cs)
    ('.':cs) -> cont TokenDot cs
    ('-':('-':cs)) -> lexer cont $ dropWhile ('\n' /=) cs
    ('{':('-':cs)) -> consumirBK 0 0 cs
    ('-':('}':cs)) -> failPos' "Comentario no abierto"
    ('-':('>':cs)) -> cont TokenArrow cs
    ('/':('\\':cs))-> cont TokenInterrupt cs
    ('(':cs) -> cont TokenOpenBrack cs
    (')':cs) -> cont TokenCloseBrack cs
    ('[':(']':cs)) -> cont TokenExternalChoice cs
    ('|':('~':('|':cs))) -> cont TokenInternalChoice cs
    ('|':('|':cs)) -> cont TokenParallel cs
    ('|':cs) -> cont TokenLabeledAlternative cs
    (';':cs) -> cont TokenSequential cs
    ('=':('=':cs)) -> maybe
            (failPos' "Se esperaba un separador")
            (cont TokenSeparator)
            (consumirSep cs)
    ('=':cs) -> cont TokenAssign cs
    ('!':cs) -> cont TokenExclamation cs
    ('?':cs) -> cont TokenQuestion cs
    ('+':cs) -> cont (TokenBinOp "+") cs
    ('-':cs) -> cont (TokenBinOp "-") cs
    ('"':(c:('"':cs))) -> cont (TokenChar c) cs
    ('"':('\\':(c:('"':cs)))) -> maybe
            (failPos' ("Sequencia de escape invalida (\\"++[c]++")"))
            (\esc -> cont (TokenChar esc) cs)
            (escapeSeq c)
    unknown -> failPos' ("No se puede reconocer " ++ take 10 unknown ++ "...")
    where
        lexWord cs = case span (includeUnderscore isAlpha) cs of
            ("STOP", rest) -> cont TokenStop rest
            ("SKIP", rest) -> cont TokenSkip rest
            (name, rest) -> {-trace name $-}
                if all (includeUnderscore isUpper) name
                then cont (TokenWORD name) rest
                else if all (includeUnderscore isLower) name
                then cont (TokenWord name) rest
                else {-trace name-} failPos' "Se esperaba un evento, proceso o indice"
        lexNumber cs = case span isNumber cs of
            (number, rest) -> cont (TokenNumber (read number)) rest
        consumirBK anidado cl cs = case cs of
            ('-':('-':css)) -> consumirBK anidado cl $ dropWhile ('\n' /=) css
            ('{':('-':css)) -> consumirBK (anidado+1) cl css
            ('-':('}':css)) -> case anidado of
                                0 -> \line -> lexer cont css (line+cl)
                                _ -> consumirBK (anidado-1) cl css
            ('\n':css) -> consumirBK anidado (cl+1) css
            (_:css) -> consumirBK anidado cl css
            [] -> cont TokenEOF []
            where
                _ = anidado :: Int
        consumirSep cs = do
            c1 <- case dropWhile (== '=') cs of
                (' ':c1) -> Just c1
                _ -> Nothing
            c2 <- case dropWhile (== ' ') c1 of
                ('O':c2) -> Just c2
                _ -> Nothing
            case dropWhile (== ' ') c2 of
                ('=':('=':c3)) -> Just (dropWhile (== '=') c3)
                _ -> Nothing
        includeUnderscore p c = p c || c == '_'

data Token
  = TokenStop
  | TokenSkip
  | TokenArrow
  | TokenExternalChoice
  | TokenInternalChoice
  | TokenParallel
  | TokenInterrupt
  | TokenExclamation
  | TokenQuestion
  | TokenSequential
  | TokenOpenBrack
  | TokenCloseBrack
  | TokenLabeledAlternative
  | TokenEOF
  | TokenAssign
  | TokenSeparator
  | TokenDot
  | TokenWord String
  | TokenWORD String
  | TokenNumber Int
  | TokenBinOp String
  | TokenChar Char
  deriving (Eq, Show)

escapeSeq :: Char -> Maybe Char
escapeSeq c = case c of
    'a' -> Just '\a'
    'b' -> Just '\b'
    'f' -> Just '\f'
    'n' -> Just '\n'
    'r' -> Just '\r'
    't' -> Just '\t'
    'v' -> Just '\v'
    '0' -> Just '\0'
    '\'' -> Just '\''
    '\"' -> Just '\"'
    '\\' -> Just '\\'
    _ -> Nothing
