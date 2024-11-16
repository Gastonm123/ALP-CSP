module Lang (ProcId, Event, Proc (..), Sentence (..), Generic (..), Expression (..), Prog (..)) where

-- Identificadores de Procesos y Eventos
type ProcId = String -- a:String { uppercase a }
type Event = String -- a:String { lowercase a }
type Variable = String

data Expression
  = Plus Expression Expression
  | Times Expression Expression
  | Minus Expression Expression
  | Div Expression Expression
  | Var Variable
  | Const Int
  deriving (Show, Eq)

-- Procesos
data Proc
  = InternalChoice Proc Proc
  | ExternalChoice Proc Proc
  | LabeledAlt Proc Proc
  | Parallel Proc Proc
  | Sequential Proc Proc
  | Prefix Event Proc
  | Interrupt Proc Proc
  | ByName ProcId
  | Stop
  | Skip
  deriving (Show, Eq)

-- Sentencias
data Sentence
  = Assign ProcId Proc
  | Eq Proc Proc
  | NEq Proc Proc
  | NEqStar Proc Proc
  deriving (Show, Eq)

data Prog = Prog {
  sentences :: [Sentence],
  events :: [Event]
}

-- DEPRECATED
-- Generic container
data Generic = SentG Sentence | ProcG Proc | Error String deriving Show -- similar a un OR
