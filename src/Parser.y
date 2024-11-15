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
  '-O-'      { TokenSeparator }
  '->'       { TokenArrow }
  '[]'       { TokenExternalChoice }
  '|~|'      { TokenInternalChoice }
  '||'       { TokenParallel }
  '|'        { TokenLabeledAlternative }
  '/\\'      { TokenInterrupt }
  ';'        { TokenSequential }
  ProcId     { TokenProcId $$ }
  Event      { TokenEvent $$ }
  '('        { TokenOpenBrack }
  ')'        { TokenCloseBrack }
  '='        { TokenAssign }
  '=='       { TokenEq }
  '/='       { TokenNEq }
  '*/=*'     { TokenNEqStar }

%left '||' ';'
%left '/\\'
%left '|~|' '[]' '|'
%right '->'

%%

Prog :: { Prog }
     : Sentences '-O-' Events

Events :: { [Event] }
       : Event Events
       | Event

Sentences :: { [Sentence] }
          : Sentence Sentences { $1 : $2 }
          | Sentence              { [$1] }

Sentence :: { Sentence }
         : ProcId '=' Proc   { Assign $1 $3 }
         | Proc '==' Proc    { Eq $1 $3 }
         | Proc '/=' Proc    { NEq $1 $3 }
         | Proc '*/=*' Proc    { NEqStar $1 $3 }

Prefix :: { Event }
       : Event       { $1 }

Proc :: { Proc }
     : Prefix '->' Proc           { Prefix $1 $3 }
     | Proc '|' Proc              {% case ($1, $3) of
                                      (Prefix _ _, Prefix _ _) -> returnP $ LabeledAlt $1 $3
                                      (Prefix _ _, LabeledAlt _ _) -> returnP $ LabeledAlt $1 $3
                                      (LabeledAlt _ _, Prefix _ _) -> returnP $ LabeledAlt $1 $3
                                      (LabeledAlt _ _, LabeledAlt _ _) -> returnP $ LabeledAlt $1 $3
                                      _ -> (failPos "Se esperaban expresiones con guarda") }
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
file_parse s = parseDecls s 1
line_parse s = parseDecl s 1
}
