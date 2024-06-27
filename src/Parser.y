{
-- |
-- Module      :  CSP.Parser
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with parsing a CSP file or CSP single line.
module Parser (
  file_parse,
  line_parse,
  ParseResult (..)
) where
import AST
import Data.Char
}

%name parseDecls Declarations
%name parseDecl Declaration

%tokentype { Token }
%error { parseError }

%lexer { lexer } { TokenEOF }
%monad { P } { thenP } { returnP }

%token
  STOP       { TokenStop }
  SKIP       { TokenSkip }
  '->'       { TokenArrow }
  '[]'       { TokenExternalChoice }
  '|~|'      { TokenInternalChoice }
  '|||'      { TokenParallel }
  '|'        { TokenLabeledAlternative }
  '/\\'      { TokenInterrupt }
  ';'        { TokenSequential }
  -- '?'        { TokenReceive }
  -- '!'        { TokenSend }
  ProcId     { TokenProcId $$ }
  Event      { TokenEvent $$ }
  '('        { TokenOpenBrack }
  ')'        { TokenCloseBrack }
  '='        { TokenEquals }

%left '|||' ';'
%left '/\\'
%left '|~|' '[]' '|'
%right '->'

%%

Declarations :: { [Sentence] }
             : Declaration Declarations { $1 : $2 }
             | Declaration              { [$1] }

Declaration :: { Sentence }
            : ProcId '=' Proc   { Assign $1 $3 }

LabeledAlt :: { [(Event, Proc)] }
           : Event '->' Proc '|' LabeledAlt1      { ($1, $3) : $5 }
           
LabeledAlt1 :: { [(Event, Proc)] }
            : Event '->' Proc                     { [($1, $3)] }
            | Event '->' Proc '|' LabeledAlt1     { ($1, $3) : $5 }

Proc :: { Proc }
     : LabeledAlt               { LabeledAlt $1 }
     | Event '->' Proc          { Prefix $1 $3 }
     | Proc '[]' Proc           { ExternalChoice $1 $3 }
     | Proc '|~|' Proc          { InternalChoice $1 $3 }
     | Proc '/\\' Proc          { Interrupt $1 $3 }
     | Proc ';' Proc            { Sequential $1 $3 }
     | Proc '|||' Proc          { Parallel $1 $3 }
     | STOP                     { Stop }
     | SKIP                     { Skip }
     | ProcId                   { ByName $1 }
     | '(' Proc ')'             { $2 }

{

data Token
  = TokenStop
  | TokenSkip
  | TokenArrow
  | TokenExternalChoice
  | TokenInternalChoice
  | TokenParallel
  | TokenInterrupt
  | TokenReceive
  | TokenSend
  | TokenProcId String
  | TokenEvent String
  | TokenSequential
  | TokenOpenBrack
  | TokenCloseBrack
  | TokenLabeledAlternative
  | TokenEOF
  | TokenEquals
  deriving (Eq, Show)

data ParseResult a = Ok a | Failed String deriving Show
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
   case m s l of
       Ok a     -> k a s l
       Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
   case m s l of
      Ok a     -> Ok a
      Failed e -> k e s l



parseError :: Token -> P a
parseError  tok s i = Failed $ "Linea "++(show i)++": Error de parseo en el token "++(show tok)++"\n"

lexer cont s = case s of
                    [] -> cont TokenEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexAlpha (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TokenArrow cs
                    ('/':('\\':cs))-> cont TokenInterrupt cs
                    ('(':cs) -> cont TokenOpenBrack cs
                    (')':cs) -> cont TokenCloseBrack cs
                    ('[':(']':cs)) -> cont TokenExternalChoice cs
                    ('|':('~':('|':cs))) -> cont TokenInternalChoice cs
                    ('|':('|':('|':cs))) -> cont TokenParallel cs
                    ('|':cs) -> cont TokenLabeledAlternative cs
                    (';':cs) -> cont TokenSequential cs
                    ('=':cs) -> cont TokenEquals cs
                    unknown -> \line -> Failed $ 
                     "Linea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexAlpha cs = case span isAlpha cs of
                              ("STOP", rest) -> cont TokenStop rest
                              ("SKIP", rest) -> cont TokenSkip rest
                              (name, rest)
                                         | all isLower name -> cont (TokenEvent name) rest
                                         | all isUpper name -> cont (TokenProcId name) rest
                                         | True -> \ line -> Failed $ "Linea "++(show line)++": Nombre invalido ( "++name++" )\n"
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                          
file_parse s = parseDecls s 1
line_parse s = parseDecl s 1
}
