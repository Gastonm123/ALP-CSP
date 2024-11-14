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
  Generic (..),
  Expression (..))
where

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
data Proc where
  InternalChoice :: Proc -> Proc -> Proc
  ExternalChoice :: Proc -> Proc -> Proc
  LabeledAlt :: Proc -> Proc -> Proc
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
  | Eq Proc Proc
  | NEq Proc Proc
  | NEqStar Proc Proc
  deriving (Show, Eq)

-- Generic container
data Generic = SentG Sentence | ProcG Proc | Error String deriving Show -- similar a un OR
