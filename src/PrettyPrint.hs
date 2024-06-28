-- |
-- Module      :  CSP.PrettyPrint
-- Copyright   :  -
-- License     :  -
-- Maintainer  :  -
-- Stability   :  -
--
-- This library deals with pretty printing errors, sentences and processes

module PrettyPrint (prettyPrint, Generic(..), errorStyle) where
import AST
import Prettyprinter.Render.Terminal
import Prettyprinter

data Generic = SentG Sentence | ProcG Proc | Error String -- similar a un OR

errorStyle :: AnsiStyle
errorStyle = color Red <> bold

-- Usamos fillSep para que se corten las lineas largas en varias lineas
prettyPrint :: Generic -> Doc AnsiStyle
prettyPrint (SentG (Assign p q)) = fillSep [pretty p, pretty "=", prettyPrint (ProcG q)]
prettyPrint (SentG (Compare p q)) = fillSep [prettyPrint (ProcG p), pretty "==", prettyPrint (ProcG q)]
prettyPrint (ProcG (InternalChoice p q)) = fillSep [prettyPrint (ProcG p), pretty "|~|", prettyPrint (ProcG q)]
prettyPrint (ProcG (ExternalChoice p q)) = fillSep [prettyPrint (ProcG p), pretty "[]", prettyPrint (ProcG q)]
prettyPrint (ProcG (Parallel p q)) = fillSep [prettyPrint (ProcG p), pretty "||", prettyPrint (ProcG q)]
prettyPrint (ProcG (Sequential p q)) = fillSep [prettyPrint (ProcG p), pretty ";", prettyPrint (ProcG q)]
prettyPrint (ProcG (Prefix pref q)) = fillSep [pretty pref, pretty "->", prettyPrint (ProcG q)]
prettyPrint (ProcG (Interrupt p q)) = fillSep [prettyPrint (ProcG p), pretty " /\\ ", prettyPrint (ProcG q)]
prettyPrint (ProcG (ByName p)) = pretty p
prettyPrint (ProcG (Paren p)) = pretty "(" <> prettyPrint (ProcG p) <> pretty ")"
prettyPrint (ProcG Stop) = pretty "STOP"
prettyPrint (ProcG Skip) = pretty "SKIP"
prettyPrint (Error err) = annotate errorStyle (pretty ("* " ++ err ++ " *"))
prettyPrint _ = error "Not implemented"

-- Con <+> se une y se agrega un espacio
-- Con <> se une sin espacio
{-
prettyPrint :: Generic -> Doc AnsiStyle
prettyPrint (SentG (Assign p q)) = pretty p <+> pretty "=" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (InternalChoice p q)) = prettyPrint (ProcG p) <+> pretty "|~|" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (ExternalChoice p q)) = prettyPrint (ProcG p) <+> pretty "[]" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (Parallel p q)) = prettyPrint (ProcG p) <+> pretty "||" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (Sequential p q)) = prettyPrint (ProcG p) <+> pretty ";" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (Prefix pref q)) = pretty pref <+> pretty "->" <+> prettyPrint (ProcG q)
prettyPrint (ProcG (Interrupt p q)) = prettyPrint (ProcG p) <+> pretty " /\\ " <+> prettyPrint (ProcG q)
prettyPrint (ProcG (ByName p)) = pretty p
prettyPrint (ProcG Stop) = pretty "STOP"
prettyPrint (ProcG Skip) = pretty "SKIP"
prettyPrint (Error err) = annotate errorStyle (pretty ("* " ++ err ++ " *"))
prettyPrint _ = error "Not implemented"
-}