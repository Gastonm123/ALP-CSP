{- Backup de codigo que movi para probar la extension LambdaCase -}
(\gen -> 
  case gen of
    SentG (Sentence p q) -> do
      let evalRes = evalProcStar defines q
      q' <- runStar evalRes events
      refusals <- refusalStar evalRes events
      
    ProcG p ->
      evalProcStar)

{- Continuation Passing Style pero no es tan interesante. Solo consegui mover
 - codigo de interactive' a la continuacion -}
{- ¿Se podra implementar recursion de cola con CPS? -}
interactiveCPS :: [Sentence] -> IO ()
interactiveCPS prog = do
  defines <- stToIO $ eval prog
  let cont = (\newProg -> do 
                            p <- newProg
                            interactiveCPS' defines p cont) -- un bucle infinito
  interactiveCPS' defines (map Sent prog) cont

interactiveCPS' :: Namespace s -> [Generic] -> (ST s [Generic] -> IO ()) -> IO ()
interactiveCPS' defines prog cont = do
  printProgState defines prog
  putStr "CSP> "
  line <- getLine
  case parseLine line of
    Right events -> let
      runProg = do -- ST s [Generic]
        forM prog (\case
          (SentG (Sentence p q)) -> do
            let evalRes = evalProcStar defines q
            runStar evalRes events
          (ProcG p) -> do
            let evalRes = evalProcStar defines p
            runStar evalRes events)
      in
        cont runProg

    Left error -> do print "!! " ++ error
                     cont (return prog)