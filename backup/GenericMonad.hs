module GenericMonad {-# DEPRECATED "GenericM no es una monada :C" #-} (GenericM(..)) where
import Control.Monad
import Control.Monad.ST

data GenericM a = GenIO IO a | GenST ST s a

thenM :: GenericM a -> (a -> GenericM b) -> GenericM b
(GenIO io) `thenM` k = GenIO (io >>= (\a -> case k a of
  GenIO io1 -> io1
  GenST st1 -> stToIO st1))
(GenST st) `thenM` k = GenIO (stToIO st) `thenM` k

returnM :: a -> GenericM a
returnM a = GenIO (return a)

instance Monad GenericM where
  return = returnM
  (>>=) = thenM