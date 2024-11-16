{
-- |
-- Module      :  CSP.Parser
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with parsing a CSP file or CSP single line.
module Parser (parseFile, parseInteractive) where
import ParserMonad
import Lang
import Lexer
}

%name parseFile Program
%name parseInteractive Sentence

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
  '('        { TokenOpenBrack }
  ')'        { TokenCloseBrack }
  '='        { TokenAssign }
  '=='       { TokenEq }
  '/='       { TokenNEq }
  '*/=*'     { TokenNEqStar }
  '.'        { TokenDot }
  BinOp      { TokenBinOp $$ }
  WORD       { TokenWORD $$ }
  word       { TokenWord $$ }
  Number     { TokenNumber $$ }

%left '||' ';'
%left '/\\'
%left '|~|' '[]' '|'
%right '->'

%%

Program :: { ([S]) }
     : Sentences '-O-' Events   { SProg $1 $2 }
     | Sentences                { SProg $1 [] }

Events :: { [SEvent] }
       : Event Events           { $1 : $2 }
       | Event                  { [$1] }

ProcRef :: { SProcRef }
        : WORD '.' Params        { $1 ++ "." ++ $3 ++ "." ++ $5 }
        | WORD                   {}

Params :: { [SParamater] }
       : Param Params            { $1 : $2 }
       | Param                   { [$1] }

Param  :: { SParamater }
       : word BinOp Number       { SOp }
Event :: { SEvent }
      : word '.' indices        { SEvent $1 $3 }
      | word                    { SEvent $1 [] }


Sentences :: { [SSentence] }
          : Sentence Sentences { $1 : $2 }
          | Sentence              { [$1] }

Sentence :: { SSentence }
         : ProcRef '=' Proc   { SAssign $1 $3 }

Prefix :: { SEvent }
       : Event       { $1 }

Proc :: { SProc }
     : Prefix '->' Proc           { SPrefix $1 $3 }
     | Proc '|' Proc              
            {% case ($1, $3) of
            (SPrefix _ _, SPrefix _ _) -> returnP $ SLabeledAlt $1 $3
            (SPrefix _ _, SLabeledAlt _ _) -> returnP $ SLabeledAlt $1 $3
            (SLabeledAlt _ _, SPrefix _ _) -> returnP $ SLabeledAlt $1 $3
            (SLabeledAlt _ _, SLabeledAlt _ _) -> returnP $ SLabeledAlt $1 $3
            _ -> (failPos "Se esperaban expresiones con guarda") }
     | Proc '[]' Proc             { SExternalChoice $1 $3 }
     | Proc '|~|' Proc            { SInternalChoice $1 $3 }
     | Proc '/\\' Proc            { SInterrupt $1 $3 }
     | Proc ';' Proc              { SSequential $1 $3 }
     | Proc '||' Proc             { SParallel $1 $3 }
     | STOP                       { SStop }
     | SKIP                       { SSkip }
     | ProcRef                    { SByName $1 }
     | '(' Proc ')'               { $2 }
