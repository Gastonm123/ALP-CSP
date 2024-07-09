module RunnableProc (ProcRep (..), RunnableProc (..)) where
{- El modulo podria llamarse RuntimeProcTAD -}

import AST ( Event, ProcId )

-- Proccess view from datatype a
data ProcRep a
  = ExternalChoiceRep a a
  | InternalChoiceRep a a
  | PrefixRep Event a
  | ParallelRep a a
  | InterruptRep a a
  | SequentialRep a a
  | ByNameRep ProcId
  | StopRep
  | SkipRep

class RunnableProc a where
  run :: a -> Event -> a
  accept :: a -> Event -> Bool
  refusal :: a -> Event -> Bool
  inAlpha :: a -> Event -> Bool
  getAlpha :: a -> [Event]
  asProc :: a -> ProcRep a
  fromProc :: ProcRep a -> a
  findProc :: a -> ProcId -> a

