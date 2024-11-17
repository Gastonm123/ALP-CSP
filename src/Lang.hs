{-# LANGUAGE GADTs #-}
module Lang (Val(..), SProg(..), SIndex, SSentence(..), BinOp, SParameter(..), SProcRef(..), SProc(..), SEvent, ProcRef(..), Parameter(..), Event(..), Index(..), Proc (..), Sentence (..), Prog (..)) where
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
  | Prefix Event Proc
  | Interrupt Proc Proc
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
  | IOp String BinOp Int

-- Valores de los indices
data Val
  = Char Char
  | Int Int
  deriving Show



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
type SEvent = Event

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
type SIndex = Index

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
  show (IOp i op c) = i ++ op ++ show c

instance Show Event where
  show (Event n i) = if not (null i) then n ++ "." ++ intercalate "." (map show i) else n

instance Show SParameter where
  show (SOp n op c) = n ++ op ++ show c
  show (SBase n) = show n

instance Show SProcRef where
  show (SProcRef n p) = if not (null p) then n ++ "." ++ intercalate "." (map show p) else n

instance Show Prog where
  show _ = "working on it......."