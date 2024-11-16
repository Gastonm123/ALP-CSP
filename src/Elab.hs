module Elab where
    
import Lang

elab :: SProg -> Prog
elab (SProg sents trace) = Prog (map elabSent sents) trace

elabSent :: SSentence -> Sentence
elabSent