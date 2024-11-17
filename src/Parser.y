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
  '.'        { TokenDot }
  '?'        { TokenQuestion }
  '!'        { TokenExclamation }
  BinOp      { TokenBinOp $$ }
  WORD       { TokenWORD $$ }
  word       { TokenWord $$ }
  Number     { TokenNumber $$ }
  Char       { TokenChar $$ }

%left '||' ';'
%left '/\\'
%left '|~|' '[]' '|'
%right '->'

%%

Program :: { SProg }
     : Sentences '-O-' Trace    { SProg $1 $3 }
     | Sentences                { SProg $1 [] }

Trace  :: { [Event] }
       : TraceEv Trace          { $1 : $2 }
       | TraceEv                { [$1] }
       | {- empty -}            { [] }

TraceEv :: { Event }
        : word '.' ValuedIndices  { Event $1 $3 }
        | word                    { Event $1 [] }

ProcRef :: { SProcRef }
        : WORD '.' Params        { SProcRef $1 $3 }
        | WORD                   { SProcRef $1 [] }

Params :: { [SParameter] }
       : Param '.' Params        { $1 : $3 }
       | Param                   { [$1] }

Param  :: { SParameter }
       : word BinOp Number       { SOp $1 $2 $3 }
       | Number                  { SBase $1 }
       | '(' Param ')'               { $2 }

Events :: { [SEvent] }
       : Event Events           { $1 : $2 }
       | Event                  { [$1] }

Event :: { SEvent }
      : word '.' Indices        { Event $1 $3 }
      | word '!' Index          { Event $1 [$3] }
      | word '?' Index          { Event $1 [$3] }
      | word                    { Event $1 [] }

Index :: { SIndex }
      : word BinOp Number       { IOp $1 $2 $3 }
      | word                    { IVar $1 }
      | Char                    { IVal (Char $1) }
      | Number                  { IVal (Int $1) }
      | '(' Index ')'           { $2 }

Indices :: { [SIndex] }
        : Index '.' Indices     { $1 : $3 }
        | Index '!' Index       { $1 : [$3] }
        | Index '?' Index       { $1 : [$3] }
        | Index                 { [$1] }

ValuedIndices :: { [Index] }
              : Number '.' ValuedIndices { (IVal (Int $1)) : $3 }
              | Number                   { [IVal (Int $1)] }
              | Char '.' ValuedIndices   { (IVal (Char $1)) : $3 }
              | Char                     { [IVal (Char $1)] }

Sentences :: { [SSentence] }
          : Sentence Sentences    { $1 : $2 }
          | Sentence              { [$1] }

Sentence :: { SSentence }
         : ProcRef '=' Proc   { SAssign $1 $3 }

Proc :: { SProc }
     : Event '->' Proc           { SPrefix $1 $3 }
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
