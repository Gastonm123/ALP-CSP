module Lexer where

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
    [] -> cont TokenEOF []
    ('\n':s)  ->  \line -> lexer cont s (line + 1)
    (c:cs)
        | isSpace c -> lexer cont cs
        | isAlpha c -> lexIdentifier (c:cs)
        | isNumber c -> lexNumber (c:cs)
    ('.':cs) -> cont TokenDot cs
    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
    ('{':('-':cs)) -> consumirBK 0 0 cont cs
    ('-':('}':cs)) -> failPos "Comentario no abierto"
    ('-':('>':cs)) -> cont TokenArrow cs
    ('/':('\\':cs))-> cont TokenInterrupt cs
    ('(':cs) -> cont TokenOpenBrack cs
    (')':cs) -> cont TokenCloseBrack cs
    ('[':(']':cs)) -> cont TokenExternalChoice cs
    ('|':('~':('|':cs))) -> cont TokenInternalChoice cs
    ('|':('|':cs)) -> cont TokenParallel cs
    ('|':cs) -> cont TokenLabeledAlternative cs
    (';':cs) -> cont TokenSequential cs
    ('=':('=':cs)) -> cont TokenEq cs
    ('=':('=':('=':cs))) -> maybe 
            (failPos "Se esperaba un separador")
            (\rest -> cont TokenSeparator rest)
            (consumirSep cs)
    ('/':('=':cs)) -> cont TokenNEq cs
    ('*':('/':('=':('*':cs)))) -> cont TokenNEqStar cs
    ('=':cs) -> cont TokenAssign cs
    ('!':cs) -> let (msg, rest) = break (\c -> c == ' ' || c == '\n') cs
            in cont (TokenSend ('!':msg)) rest 
    ('?':cs) -> let (msg, rest) = break (\c -> c == ' ' || c == '\n') cs
            in cont (TokenReceive ('?':msg)) rest
    unknown -> \line -> failPos ("No se puede reconocer " ++ (take 10 unknown) ++ "...")
    where lexIdentifier cs = case span (allowedChars [isAlphaNum, (==) '_', (==) '.']) cs of
            ("STOP", rest) -> cont TokenStop rest
            ("SKIP", rest) -> cont TokenSkip rest
            (name, rest)
                        | isNumber (head name) -> lexIndex name rest
                        | isLower (head name)
                        && all
                            (allowedChars [isLower, isNumber, (==) '_', (==) '.'])
                            name -> cont (TokenEvent name) rest
                        | isUpper (head name)
                        && all
                            (allowedChars [isUpper, isNumber, (==) '_', (==) '.'])
                            name -> cont (TokenProcId name) rest
                        | otherwise -> failPos ("Nombre invalido ( " ++ name ++ " )")
        lexIndex cs rest = case span isNumber cs of
            (index, '.':name)
                        | isLower (head name)
                        && all
                            (allowedChars [isLower, isNumber, (==) '_', (==) '.'])
                            name -> cont (TokenEvent cs) rest
                        | isUpper (head name)
                        && all
                            (allowedChars [isUpper, isNumber, (==) '_', (==) '.'])
                            name -> cont (TokenProcId cs) rest
            _ -> failPos ("Nombre invalido ( " ++ cs ++ " )")
        consumirBK anidado cl cont s = case s of
            ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
            ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
            ('-':('}':cs)) -> case anidado of
                                0 -> \line -> lexer cont cs (line+cl)
                                _ -> consumirBK (anidado-1) cl cont cs
            ('\n':cs) -> consumirBK anidado (cl+1) cont cs
            (_:cs) -> consumirBK anidado cl cont cs
        allowedChars :: [Char -> Bool] -> Char -> Bool
        allowedChars allowed x = or (map (\f -> f x) allowed)
        consumirSep cs = do
            case dropWhile (== '=') cs of
            (' ':c1) -> Just c1
            _ -> Nothing
            \c1 -> case dropWhile (== ' ') c1 of
            ('O':c2) -> Just c2
            _ -> Nothing
            \c2 -> case dropWhile (== ' ') c2 of
            ('=':'=':'=':c3) -> Just (dropWhile (== '=') c3)
            _ -> Nothing