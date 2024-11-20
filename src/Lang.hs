{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lang (Val(..), SProg(..), SIndex(..), SSentence(..), BinOp, SParameter(..), SProcRef(..), SProc(..), SEvent(..), ProcRef(..), Parameter(..), Event(..), Index(..), Proc (..), Sentence (..), Prog (..)) where
import Data.List (intercalate)

-- Programa
data Prog =
  Prog {
    sentences :: [Sentence],
    events :: [Event]
  }

-- Referencias a procesos
data ProcRef
  = ProcRef {
    procName :: String,
    params   :: [Parameter]
  }

-- Eventos
data Event =
  Event {
    eventName :: String,
    indices   :: [Index]
  }

-- Procesos
data Proc
  = InternalChoice Proc Proc
  | ExternalChoice Proc Proc
  | LabeledAlt Proc Proc
  | Parallel Proc Proc
  | Sequential Proc Proc
  | Interrupt Proc Proc
  | Prefix Event Proc
  | ByName ProcRef
  | Stop
  | Skip
  deriving Show

-- Sentencias
data Sentence
  = Assign ProcRef Proc
  deriving Show


-- Parametros de un proceso. Un parametro puede ser
-- inductivo o definir una referencia a un valor fijo
data Parameter
  = Inductive String Int -- es inductivo si tiene una variable
                         -- (n, n+1, n+2, etc.)
  | Base Int

-- Indices
data Index
  = IVal Val
  | IVar String
  | IPlus String Int

-- Valores de los indices
data Val
  = Char Char
  | Int Int
  deriving (Show, Eq)



-- Syntax sugaring
-- Programa azucarado
data SProg =
  SProg {
    ssentences :: [SSentence],
    sevents :: [Event]
  }

-- Referencias a procesos azucaradas
data SProcRef =
  SProcRef {
    sprocName :: String,
    sparams   :: [SParameter]
  }

-- Eventos azucarados
data SEvent =
  SEvent {
    seventName :: String,
    sindices :: [SIndex]
  }

-- Procesos azucarados
data SProc
  = SInternalChoice SProc SProc
  | SExternalChoice SProc SProc
  | SLabeledAlt SProc SProc
  | SParallel SProc SProc
  | SSequential SProc SProc
  | SPrefix SEvent SProc
  | SInterrupt SProc SProc
  | SByName SProcRef
  | SStop
  | SSkip
  deriving Show

-- Sentencias azucaradas
data SSentence
  = SAssign SProcRef SProc
  deriving Show

-- Parametros azucarados
data SParameter
  = SOp String BinOp Int
  | SBase Int

-- Indices azucarados
data SIndex
  = SIVar String
  | SIVal Val
  | SIOp String BinOp Int

-- Otros
type BinOp = String


-- DEPRECATED
-- Generic container
-- data Generic = SentG Sentence | ProcG Proc | Error String deriving Show -- similar a un OR


instance Show Parameter where
  show (Inductive n c) = n ++ show c
  show (Base n) = show n

instance Show ProcRef where
  show (ProcRef n p) = if not (null p) then n ++ "." ++ intercalate "." (map show p) else n

instance Show Index where
  show (IVal i) = show i
  show (IVar n) = n
  show (IPlus i c) = i ++ "+" ++ show c

instance Show Event where
  show (Event n i) = if not (null i) then n ++ "." ++ intercalate "." (map show i) else n

instance Show SParameter where
  show (SOp n op c) = n ++ op ++ show c
  show (SBase n) = show n

instance Show SProcRef where
  show (SProcRef n p) = if not (null p) then n ++ "." ++ intercalate "." (map show p) else n

instance Show Prog where
  show _ = "working on it......."

instance Show SIndex where
  show (SIOp m op c) = m ++ op ++ show c
  show (SIVar m) = m
  show (SIVal v) = show v

instance Show SEvent where
  show (SEvent n i) = if not (null i) then n ++ "." ++ intercalate "." (map show i) else n


-- Igualdad de eventos y procesos:
-- Si existe una asignacion donde son iguales, entonces son iguales

pattern INF = -1
data Dom = DomInt (Int, Int) | DomChar Char

instance Eq Event where
  (==) (Event n1 is1) (Event n2 is2)
    =  n1 == n2
    && length is1 == length is2
    && not (isEmptyInters (map dom is1) (map dom is2))
    where
      dom (IVal (Char c)) = DomChar c
      dom (IVal  (Int v)) = DomInt (v,v)
      dom (IVar        _) = DomInt (0,INF)
      dom (IPlus     _ c) = DomInt (c,INF)

instance Eq ProcRef where
  (==) (ProcRef n1 pars1) (ProcRef n2 pars2)
    =  n1 == n2
    && length pars1 == length pars2
    && not (isEmptyInters (map dom pars1) (map dom pars2))
    where
      dom (Base        v) = DomInt (v,v)
      dom (Inductive _ c) = DomInt (c,INF)


isEmptyInters :: [Dom] -> [Dom] -> Bool
isEmptyInters ((DomChar c1):d1) ((DomChar c2):d2) =
    (c1 /= c2) && isEmptyInters d1 d2
isEmptyInters ((DomInt (_,INF)):_) ((DomInt (_,INF)):_) = False
isEmptyInters ((DomInt (n,_)):d1) ((DomInt (m,INF)):d2) =
    (n < m) && isEmptyInters d1 d2
isEmptyInters ((DomInt (n,INF)):d1) ((DomInt (m,_)):d2) =
    (m < n) && isEmptyInters d1 d2
isEmptyInters [] [] = True
isEmptyInters _ _ = True -- caso DomChar DomInt y viceversa
-- isEmptyInters puede pensarse como la interseccion de dos subconjuntos de N^r
-- (producto cartesiano de N, r veces).
-- En el caso de los indices el producto cartesiano puede incluir singletes de
-- caracteres.