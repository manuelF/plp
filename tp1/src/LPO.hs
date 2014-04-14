module LPO where

import Data.Char
import Data.List

type Nombre = String

data Termino = Var Nombre 
             | Func Nombre [Termino]

data Formula = Pred Nombre [Termino] 
             | No Formula 
             | Y Formula Formula 
             | O Formula Formula 
             | Imp Formula Formula 
             | A Nombre Formula 
             | E Nombre Formula


---------------
-- Ejercicio 1
---------------

esLiteral :: Formula -> Bool
esLiteral (Pred _ _)        = True
esLiteral (No (Pred _ _))   = True
esLiteral _                 = False


---------------
-- Ejercicio 2
---------------

foldTermino  :: (Nombre -> b) 
             -> (Nombre -> [b] -> b) 
             -> Termino 
             -> b 
foldTermino fVar fTer ter = 
    case ter of
        Var n       -> fVar n
        Func n ter' -> fTer n (map rec ter') 
    where rec = foldTermino fVar fTer


---------------
-- Ejercicio 3
---------------

foldFormula :: (Nombre -> [Termino] -> b)
            -> (b -> b)
            -> (b -> b -> b)
            -> (b -> b -> b)
            -> (b -> b -> b)
            -> (Nombre -> b -> b)
            -> (Nombre -> b -> b)
            -> Formula
            -> b
foldFormula fPred fNo fY fO fImp fA fE f = 
    case f of
        Pred n ter  -> fPred n ter
        No f1       -> fNo (rec f1)
        Y f1 f2     -> fY (rec f1) (rec f2)
        O f1 f2     -> fO (rec f1) (rec f2)
        Imp f1 f2   -> fImp (rec f1) (rec f2)
        A n f1      -> fA n (rec f1)
        E n f1      -> fE n (rec f1)
    where rec = foldFormula fPred fNo fY fO fImp fA fE 


---------------
-- Ejercicio 4
---------------

recFormula :: (Nombre -> [Termino] -> b)
           -> (Formula -> b -> b)
           -> (Formula -> Formula -> b -> b -> b)
           -> (Formula -> Formula -> b -> b -> b)
           -> (Formula -> Formula -> b -> b -> b)
           -> (Formula -> Nombre -> b -> b)
           -> (Formula -> Nombre -> b -> b)
           -> Formula
           -> b
recFormula = error "Falta implementar."


---------------
-- Ejercicio 5
---------------

instance Show Termino where
  show = foldTermino wordToUpper parentizar

wordToUpper :: String -> String
wordToUpper = (map toUpper)

join::[a]->[[a]]->[a]
join separador = foldr (\x res->if null res then x else x++separador++res) []

parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s++"("++(join "," res)++")"


---------------
-- Ejercicio 6
---------------

instance Show Formula where
  show = foldFormula showPred showNeg showAnd showOr showImp showAll showExists

showPred :: Nombre -> [Termino] -> String
showPred n terms = parentizar (wordToUpper n) (map show terms)
    
showNeg :: String -> String 
showNeg = (\s -> if (esReprDeLiteral s) then "¬" ++ s else "¬(" ++ s ++ ")")

operadoresLogicos = ["¬","∧","∨","⊃","∀","∃"]
esReprDeLiteral :: String -> Bool
esReprDeLiteral s = null (filter (\x -> isInfixOf x s) operadoresLogicos)

showAnd :: String -> String -> String
showAnd = (\s1 s2 -> s1 ++ "∧" ++ s2)

showOr :: String -> String -> String
showOr = (\s1 s2 -> s1 ++ "∨" ++ s2)

showImp :: String -> String -> String
showImp = (\s1 s2 -> s1 ++ "⊃" ++ s2)

showAll :: Nombre -> String -> String
showAll = (\n s -> "∀" ++ (wordToUpper n) ++ ".(" ++ s ++ ")")

showExists :: Nombre -> String -> String
showExists = (\n s -> "∃" ++ (wordToUpper n) ++ ".(" ++ s ++ ")")


---------------
-- Ejercicio 7
---------------

eliminarImplicaciones :: Formula->Formula
eliminarImplicaciones = foldFormula Pred No Y O cambiarImp A E

cambiarImp :: Formula -> Formula -> Formula
cambiarImp f1 f2 = O (No f1) f2


---------------
-- Ejercicio 8
---------------

aFNN :: Formula -> Formula
aFNN = (cancelarNegaciones).(eliminarImplicaciones).(foldFormula Pred normalizarNegaciones Y O Imp A E)

normalizarNegaciones :: Formula -> Formula
normalizarNegaciones = (\f -> if (esLiteral f) then No f else hundirNegaciones f)

hundirNegaciones :: Formula -> Formula
hundirNegaciones = foldFormula Pred negarNo negarY negarO negarImp negarE negarA
        
negarNo :: Formula -> Formula
negarNo = (\f -> if esLiteral f then No f else f)

negarY :: Formula -> Formula -> Formula
negarY = (\f1 f2 -> O (No f1) (No f2))

negarO :: Formula -> Formula -> Formula
negarO = (\f1 f2 -> Y (No f1) (No f2))

negarImp :: Formula -> Formula -> Formula
negarImp = (\f1 f2 -> Y f1 (No f2))

negarE :: Nombre -> Formula -> Formula
negarE = (\n f -> E n (No f))

negarA :: Nombre -> Formula -> Formula
negarA = (\n f -> A n (No f))

cancelarNegaciones  :: Formula -> Formula
cancelarNegaciones = foldFormula Pred auxNo Y O Imp A E

auxNo :: Formula -> Formula
auxNo = (\f -> if estaNegada f then removerNegacion f else No f)

estaNegada :: Formula -> Bool
estaNegada = foldFormula 
    (\n terms -> False)
    (\f -> True)
    (\f1 f2 -> False)
    (\fi f2 -> False)
    (\f1 f2 -> False)
    (\n f -> False)
    (\n f -> False)

removerNegacion :: Formula -> Formula
removerNegacion = foldFormula Pred (\f->f) Y O Imp E A

---------------
-- Ejercicio 9
---------------
---------------
-- Ejercicio 10
---------------
---------------
-- Ejercicio 11
---------------
---------------
-- Ejercicio 12
---------------

