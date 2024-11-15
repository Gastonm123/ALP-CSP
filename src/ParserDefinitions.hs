{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module ParserDefinitions (
  ParseResult(..),
  P,
  thenP,
  returnP,
  failP,
  catchP,
  parseError
) where


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
  | TokenAssign
  | TokenEq
  | TokenNEq
  | TokenNEqStar
  | TokenSeparator
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
returnP a = \_ _ -> Ok a

failP :: String -> P a
failP err = \_ _ -> Failed err

failPos :: String -> LineNumber -> ParseResult
failPos err = \line -> Failed $ "Linea " ++ (show line) ++ ": " ++ err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
   case m s l of
      Ok a     -> Ok a
      Failed e -> k e s l

parseError :: (Show t) => t -> P a
parseError  tok _ i = Failed $ "Linea "++show i++": Error de parseo en el token "++show tok
