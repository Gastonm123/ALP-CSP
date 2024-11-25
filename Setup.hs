import Distribution.Simple
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Compiler
import Distribution.Types.LocalBuildInfo
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Program
import Debug.Trace

main = defaultMainWithHooks simpleUserHooks { hookedPreProcessors = customPreProcessors }

customPreProcessors =
    map changeHappy knownSuffixHandlers

changeHappy ("y", _) = ("y", debugHappy)
changeHappy otro = otro

debugHappy :: a -> LocalBuildInfo -> ComponentLocalBuildInfo -> PreProcessor
debugHappy _ lbi _ = trace "Agregando banderas de debug a Happy" (pp{platformIndependent = True})
  where
    pp = standardPP lbi happyProgram (hcFlags hc)
    hc = compilerFlavor (compiler lbi)
    hcFlags GHC = ["-agc"]
    hcFlags GHCJS = ["-agc"]
    hcFlags _ = []

standardPP :: LocalBuildInfo -> Program -> [String] -> PreProcessor
standardPP lbi prog args =
  PreProcessor {
    platformIndependent = False,
    ppOrdering = unsorted,
    runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
      runDbProgram verbosity prog (withPrograms lbi)
                           (args ++ ["-o", outFile, inFile])
  }