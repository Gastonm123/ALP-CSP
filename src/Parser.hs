module Parser(parseFile) where

{- Modulo dummy para callar al language server -}
import ParserMonad
parseFile = failP "wrong module"