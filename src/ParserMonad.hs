{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module ParserMonad (ParseResult(..), P, thenP, returnP, failP, catchP, parseError, failPos, failPos') where

import GHC.Stack
import Debug.Trace

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

failPos :: String -> P a
failPos err = \line _ -> Failed $ "Linea " ++ show line ++ ": " ++ err

failPos' :: String -> LineNumber -> ParseResult a
failPos' err = \line -> Failed $ "Linea " ++ show line ++ ": " ++ err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
   case m s l of
      Ok a     -> Ok a
      Failed e -> k e s l

parseError :: (Show t, HasCallStack) => t -> P a
parseError  tok s i = let
   stack = callStack
   in trace (prettyCallStack stack) (Failed $ "Linea "++show i++": Error de parseo en el token "++show tok++". Contexto: "++take 20 s)
