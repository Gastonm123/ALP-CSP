module Alpha (alpha) where

import Data.Function.Memoize
import Data.Map
import Data.Set
import AST

type Namespace = Map.Map String [Proc]
type Seen = Set.Set String

deriveMemoizable ''Proc
deriveMemoizable ''Namespace
deriveMemoizable ''Seen

alpha :: Namespace -> Seen -> Proc -> [Event]
alpha = memoizeFix3 alpha'

alpha' :: (Namespace -> Seen -> Proc -> [Event]) -> Namespace -> Seen -> Proc -> [Event]
alpha' f ns seen (InternalChoice p q) = (++) <$> f ns seen p <*> f ns seen q
{- Es posible evitar tanta verborragia de monadas de esta forma
alpha' ns seen (InternalChoice p q) = (liftA2 ++) <$> alpha' seen p <*> alpha' seen q
-}
alpha' f ns seen (ExternalChoice p q) = (++) <$> f ns seen p <*> f ns seen q
alpha' f ns seen (Parallel p q) = (++) <$> f ns seen p <*> f ns seen q
alpha' f ns seen (Sequential p q) = (++) <$> f ns seen p <*> f ns seen q
alpha' f ns seen (Prefix pref q) = (:) <$> pref <*> f ns seen q
alpha' f ns seen (Interrupt p q) = (++) <$> f ns seen p <*> f ns seen q
alpha' f ns seen (ByName p) =
  if Set.member p seen 
    then []
    else case Map.lookup p ns of
      Just p -> f ns (Set.insert p seen)
      Nothing -> error
        "Evaluation error: Current process has an undefined process ( " ++ p ++ " )\n"
alpha' _ _ _ Stop = []
alpha' _ _ _ Skip = []