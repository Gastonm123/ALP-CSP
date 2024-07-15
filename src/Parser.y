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
  ParseResult (..),
  P(..)
) where
import ParserDefinitions
import ParserChannel
import AST
import Data.Char
}

%name parseDecls Sentences
%name parseDecl Sentence

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
  '||'       { TokenParallel }
  '|'        { TokenLabeledAlternative }
  '/\\'      { TokenInterrupt }
  ';'        { TokenSequential }
  '?'        { TokenReceive $$ }
  '!'        { TokenSend $$ }
  ProcId     { TokenProcId $$ }
  Event      { TokenEvent $$ }
  '('        { TokenOpenBrack }
  ')'        { TokenCloseBrack }
  '='        { TokenEquals }
  '=='       { TokenCompare }

%left '||' ';'
%left '/\\'
%left '|~|' '[]' '|'
%right '->'

%%

Sentences :: { [Sentence] }
          : Sentence Sentences { $1 : $2 }
          | Sentence              { [$1] }

Sentence :: { Sentence }
         : ProcId '=' Proc   { Assign $1 $3 }
         | Proc '==' Proc    { Compare $1 $3 }

LabeledAlt :: { Proc }
           : LabeledAlt '|' LabeledAlt      { ExternalChoice $1 $3 }
           | Prefix '->' Proc               { Prefix $1 $3 }

Prefix :: { Prefix }
       : Event '?'   {% (\s l -> case parseMsg $2 l of
                           Ok expression -> case expression of 
                                 Var v -> Ok (ChannelIn $1 v)
                                 _ -> Failed $ 
                                      "Linea "++(show l)
                                      ++": El canal "++(show $1)++(show $2)
                                      ++" no esta bien definido"
                           Failed err -> Failed err) }
       | Event '!'   {% (\s l -> case parseMsg $2 l of
                           Ok expression -> Ok $ ChannelOut $1 expression
                           Failed err -> Failed err) }
       | Event       { Event $1 }

Proc :: { Proc }
     : Prefix '->' Proc           { Prefix $1 $3 }
     | LabeledAlt '|' LabeledAlt  { ExternalChoice $1 $3 }
     | Proc '[]' Proc             { ExternalChoice $1 $3 }
     | Proc '|~|' Proc            { InternalChoice $1 $3 }
     | Proc '/\\' Proc            { Interrupt $1 $3 }
     | Proc ';' Proc              { Sequential $1 $3 }
     | Proc '||' Proc             { Parallel $1 $3 }
     | STOP                       { Stop }
     | SKIP                       { Skip }
     | ProcId                     { ByName $1 }
     | '(' Proc ')'               { $2 }

{

data Token
  = TokenStop
  | TokenSkip
  | TokenArrow
  | TokenExternalChoice
  | TokenInternalChoice
  | TokenParallel
  | TokenInterrupt
  | TokenReceive String
  | TokenSend String
  | TokenProcId String
  | TokenEvent String
  | TokenSequential
  | TokenOpenBrack
  | TokenCloseBrack
  | TokenLabeledAlternative
  | TokenEOF
  | TokenEquals
  | TokenCompare
  deriving (Eq, Show)

lexer :: (Token -> P a) -> P a
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
                    ('|':('|':cs)) -> cont TokenParallel cs
                    ('|':cs) -> cont TokenLabeledAlternative cs
                    (';':cs) -> cont TokenSequential cs
                    ('=':('=':cs)) -> cont TokenCompare cs
                    ('=':cs) -> cont TokenEquals cs
                    ('!':cs) -> let (msg, rest) = break (\c -> c == ' ' || c == '\n') cs
                                in cont (TokenSend ('!':msg)) rest 
                    ('?':cs) -> let (msg, rest) = break (\c -> c == ' ' || c == '\n') cs
                                in cont (TokenReceive ('?':msg)) rest
                    unknown -> \line -> Failed $ 
                     "Linea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexAlpha cs = case span isAlphaNum cs of
                              ("STOP", rest) -> cont TokenStop rest
                              ("SKIP", rest) -> cont TokenSkip rest
                              (name, rest)
                                         | isLower (head name)
                                            && all ((||) <$> isLower <*> isNumber) name -> cont (TokenEvent name) rest
                                         | isUpper (head name)
                                            && all ((||) <$> isUpper <*> isNumber) name -> cont (TokenProcId name) rest
                                         | True -> \ line -> Failed $ "Linea "++(show line)++": Nombre invalido ( "++name++" )"
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
