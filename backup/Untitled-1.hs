import Data.Time.Clock
import Data.Time.Clock.System
import Prettyprinter

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

z = 30
a = fib 30 + fib 30 + fib 30
b z = fib z + fib z + fib z

bench f = do
  now <- getSystemTime
  print f
  then' <- getSystemTime
  let utcNow = systemToUTCTime now
  let utcThen = systemToUTCTime then'
  let elapsed = diffUTCTime utcThen utcNow
  print (pretty "elapsed:" <+> pretty (show elapsed))