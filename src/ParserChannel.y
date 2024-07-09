{
module ParserChannel (
  parseMsg,
  P(..)
) where
import ParserDefinitions
import Data.Char
import AST
}

%name parseMsg Message

%tokentype { Token }
%error { parseError }

%lexer { lexer } { TokenEOF }
%monad { P } { thenP } { returnP }

%token
  '?'        { TokenSend }
  '!'        { TokenReceive }
  '+'        { TokenPlus }
  '-'        { TokenMinus }
  '*'        { TokenTimes }
  '/'        { TokenDiv }
  Variable   { TokenVariable $$ }
  Number     { TokenNumber $$ }

%left '+' '-'
%left '*' '/'
%nonassoc '?' '!'

%%

Message :: { Expression }
        : '?' Expression   { $2 }
        | '!' Expression   { $2 }

Expression :: { Expression }
            : Expression '+' Expression   { Plus $1 $3 }
            | Expression '-' Expression   { Minus $1 $3 }
            | Expression '*' Expression   { Times $1 $3 }
            | Expression '/' Expression   { Div $1 $3 }
            | Variable                    { Var $1 }
            | Number                      { Const $1 }

{

data Token
  = TokenSend
  | TokenReceive
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenVariable String
  | TokenNumber Int
  | TokenEOF
  deriving (Eq, Show)

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
                  [] -> cont TokenEOF []
                  ('\n':s)  ->  \line -> lexer cont s (line + 1)
                  (c:cs)
                        | isSpace c -> lexer cont cs
                        | isAlpha c -> let (var, rest) = span ((||) <$> isAlpha <*> isNumber) s
                                       in cont (TokenVariable var) rest
                        | isNumber c -> let (number, rest) = span isNumber s
                                        in cont (TokenNumber (read number)) rest
                  ('*':cs) -> cont TokenTimes cs
                  ('/':cs) -> cont TokenDiv cs
                  ('+':cs) -> cont TokenPlus cs
                  ('-':cs) -> cont TokenMinus cs
                  ('!':cs) -> cont TokenSend cs
                  ('?':cs) -> cont TokenReceive cs
                  unknown -> \line -> Failed $ 
                     "Linea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
}