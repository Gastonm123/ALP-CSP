{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  AST
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library provides the structure of abstract syntax trees

module AST (
  ProcId,
  Event,
  Proc (..),
  Sentence (..),
  Generic (..))
where

-- Identificadores de Procesos y Eventos
type ProcId = String -- a:String { uppercase a }
-- type Variable = String -- a:String { lowercase a }

type Event = String -- a:String { lowercase a }

-- Procesos
data Proc where
  InternalChoice :: Proc -> Proc -> Proc
  ExternalChoice :: Proc -> Proc -> Proc
  Parallel :: Proc -> Proc -> Proc
  Sequential :: Proc -> Proc -> Proc
  Prefix :: Event -> Proc -> Proc
  Interrupt :: Proc -> Proc -> Proc
  ByName :: ProcId -> Proc
  Stop :: Proc
  Skip :: Proc

deriving instance Show Proc

deriving instance Eq Proc

-- Sentencias
data Sentence
  = Assign ProcId Proc
  | Compare Proc Proc
  deriving (Show, Eq)

-- Generic container
data Generic = SentG Sentence | ProcG Proc | Error String -- similar a un OR

-- data Error = DivByZero | UndefVar deriving (Eq, Show)

-- type Trace = String
