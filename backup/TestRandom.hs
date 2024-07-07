module TestRandom where
import System.Random
import System.Random.Stateful
import Control.Monad.ST

testRandom :: RandomGen g => Int -> g -> IO ()
testRandom 0 g = do
  let (r, _) = ((random :: RandomGen g => g -> (Int, g)) g) 
  print r
testRandom n g = do
  let (r, g') = ((random :: RandomGen g => g -> (Int, g)) g) 
  print r
  testRandom (n-1) g'
  testRandom (n-1) g'


runTest = do
  g <- initStdGen
  frozenRef <- stToIO $ do
    ref  <- newSTGenM g
    freezeGen ref
  testRandom 4 frozenRef


main :: IO ()
main = do
    -- Crear un generador de números aleatorios inicial
    gen <- getStdGen

    -- Dividir el generador original en dos generadores independientes
    let (gen1, gen2) = split gen
    
    -- Generar un número aleatorio con el generador inicial después de dividirlo
    let (valueInitial, gen') = random gen :: (Int, StdGen)

    -- Generar un número aleatorio con cada uno de los nuevos generadores
    let (value1, _) = random gen1 :: (Int, StdGen)
    let (value2, _) = random gen2 :: (Int, StdGen)

    putStrLn $ "Valor generado por el generador inicial después de dividirlo: " ++ show valueInitial
    putStrLn $ "Valor generado por gen1: " ++ show value1
    putStrLn $ "Valor generado por gen2: " ++ show value2

    -- Verificar que los valores son distintos
    if valueInitial /= value1 && valueInitial /= value2
        then putStrLn "Los valores generados por los subgeneradores son distintos del generador inicial."
        else putStrLn "Los valores generados por los subgeneradores NO son distintos del generador inicial."
