module Lang (SProg(..), SIndex, SSentence(..), BinOp, SParameter(..), SProcRef(..), SProc(..), SEvent, ProcRef(..), Parameter(..), Event(..), Index(..), Proc (..), Sentence (..), Generic (..), Prog (..)) where
import Data.List (intercalate)

-- Parametros de un proceso. Un parametro puede ser
-- inductivo o definir una referencia a un caso 
-- especifico
data Parameter
  = Inductive String Int
  | Base String
  deriving Eq

-- Referencias a procesos
data ProcRef
  = ProcRef {
    procName :: String,
    params   :: [Parameter]
  }
  deriving Eq

-- Eventos
data Index
  = Index String
  | IOp String BinOp Int
  deriving Eq

data Event =
  Event {
    eventName :: String,
    indices   :: [Index]
  }
  deriving Eq

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
  deriving (Show, Eq)

-- Sentencias
data Sentence
  = Assign ProcRef Proc
  deriving (Show, Eq)

-- Programa
data Prog = 
  Prog {
    sentences :: [Sentence],
    events :: [Event]
  }


-- Syntax sugaring
type BinOp = String
data SParameter 
  = SOp String BinOp Int
  | SBase String
  deriving Eq
  
data SProcRef =
  SProcRef {
    sprocName :: String,
    sparams   :: [SParameter]
  }
  deriving Eq

type SIndex = Index
type SEvent = Event

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
  deriving (Show, Eq)

data SSentence
  = SAssign SProcRef SProc
  deriving (Show, Eq)

data SProg =
  SProg {
    ssentences :: [SSentence],
    sevents :: [Event]
  }

-- DEPRECATED
-- Generic container
data Generic = SentG Sentence | ProcG Proc | Error String deriving Show -- similar a un OR


instance Show Parameter where
  show (Inductive n c) = n ++ show c
  show (Base n) = n

instance Show ProcRef where
  show (ProcRef n p) = if not (null p) then n ++ "." ++ intercalate "." (map show p) else n

instance Show Index where
  show (Index i) = i
  show (IOp i op c) = i ++ op ++ show c

instance Show Event where
  show (Event n i) = if not (null i) then n ++ "." ++ intercalate "." (map show i) else n

instance Show SParameter where
  show (SOp n op c) = n ++ op ++ show c
  show (SBase n) = n

instance Show SProcRef where
  show (SProcRef n p) = if not (null p) then n ++ "." ++ intercalate "." (map show p) else n
