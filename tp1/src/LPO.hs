module LPO where

import Data.Char
import Data.List

type Nombre = String

data Termino = Var Nombre | Func Nombre [Termino]

data Formula = Pred Nombre [Termino] | No Formula | Y Formula Formula | O Formula Formula | Imp Formula Formula | A Nombre Formula | E Nombre Formula

esLiteral :: Formula -> Bool
esLiteral (Pred _ _) = True
esLiteral (No _)     = True
esLiteral _          = False


foldTermino :: (Nombre -> b)            --casoVar 
               -> (Nombre -> [b] -> b)  --casoFunc
               -> Termino -> b          --termino
foldTermino casoVar casoFunc ter =
  case ter of
    Var n     -> casoVar n
    Func n ts -> casoFunc n (map rec ts)
  where rec = foldTermino casoVar casoFunc


foldFormula :: (Nombre -> [Termino] -> b)--casoPred 
               -> (b -> b)               --casoNo
               -> (b -> b -> b)          --casoY
               -> (b -> b -> b)          --casoO
               -> (b -> b -> b)          --casoImp
               -> (Nombre -> b -> b)     --casoA
               -> (Nombre -> b -> b)     --casoE
               -> Formula                --parametro
               -> b
foldFormula casoPred casoNo casoY casoO casoImp casoA casoE f =
  case f of
    Pred n ts -> casoPred n ts
    No f1     -> casoNo (rec f1)
    Y f1 f2   -> casoY (rec f1) (rec f2)
    O f1 f2   -> casoO (rec f1) (rec f2)
    Imp f1 f2 -> casoImp (rec f1) (rec f2)
    A n f1    -> casoA n (rec f1)
    E n f1    -> casoE n (rec f1)
  where rec = foldFormula casoPred casoNo casoY casoO casoImp casoA casoE


--Esquema de recursión primitiva para fórmulas.
recFormula
  :: (Nombre -> [Termino] -> b)
     -> (Formula -> b -> b)
     -> (Formula -> Formula -> b -> b -> b)
     -> (Formula -> Formula -> b -> b -> b)
     -> (Formula -> Formula -> b -> b -> b)
     -> (Formula -> Nombre -> b -> b)
     -> (Formula -> Nombre -> b -> b)
     -> Formula
     -> b
recFormula f1 f2 f3 f4 f5 f6 f7 = g 
  where
    g (Pred n t) = f1 n t
    g (No a) = f2 a (g a) 
    g (Y a b) = f3 a b (g a) (g b) 
    g (O a b) = f4 a b (g a) (g b)
    g (Imp a b) = f5 a b (g a) (g b)
    g (A n a) = f6 a n (g a)
    g (E n a) = f7 a n (g a) 


instance Show Termino where
  --show = error "Falta implementar."
  --show = foldTermino id (\n r -> parentizar n r)
  show = terminoToString
  
terminoToString :: Termino -> String
terminoToString = foldTermino mayusculirizar
                              (\n r -> if null r then n else parentizar n r)

mayusculirizar ::Nombre -> Nombre
mayusculirizar n = [toUpper l | l <- n]


join::[a]->[[a]]->[a]
join separador = foldr (\x res->if null res then x else x++separador++res) []

{- Toma un nombre de función y una lista de argumentos ya convertidos en String, y termina de armar la representación visual. -}
parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s++"("++(join "," res)++")"

instance Show Formula where
-- Operadores lógicos: "¬","∧","∨","⊃","∀","∃"
    --show = error "Falta implementar."
    show = formulaToString

formulaToString :: Formula -> String
formulaToString = foldFormula
                    (\n ts  -> parentizar (mayusculirizar n) (map terminoToString ts))
                    (\r     -> "¬" ++ r)
                    (\r1 r2 -> r1 ++ "∧" ++ r2)
                    (\r1 r2 -> r1 ++ "∨" ++ r2)
                    (\r1 r2 -> r1 ++ "⊃" ++ r2)
                    (\n r   -> "∀" ++ (mayusculirizar n) ++ ".("  ++ r ++ ")")
                    (\n r   -> "∃" ++ (mayusculirizar n) ++ ".(" ++  r ++ ")")


--Ejemplo: A "x" (Imp (Pred "p" [Var "x"]) (Pred "p" [Var "x"])) se ve como ∀X.((P(X)⊃P(X)))

--eliminarImplicaciones :: Dar tipo e implementar.
eliminarImplicaciones :: Formula -> Formula
eliminarImplicaciones  = foldFormula Pred No Y O (\r1 r2 -> O (No r1) r2) A E

aFNN::Formula->Formula
aFNN = error "Falta implementar."

--fv:: Dar tipo e implementar.

--Interpretación en un dominio a. Una función para términos y otra para predicados.
--Basta con que las funciones estén bien definidas para su dominio esperado.
data Interpretacion a = I {fTerm :: (Nombre->[a]->a), fPred :: (Nombre->[a]->Bool)}

--Ejemplo para pruebas:
ejemploNat::Interpretacion Int
ejemploNat = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "0" = const 0
		    | nombreF == "suc" = \xs -> head xs + 1
		    | nombreF == "suma" = sum
  fPredicados nombreP | nombreP == "esCero" = \xs -> head xs == 0
		      | nombreP == "esPar" = \xs -> mod (head xs) 2 == 0
		      | nombreP == "mayor" = \xs -> (head xs) > (head (tail xs))
		      | nombreP == "menor" = \xs -> (head xs) < (head (tail xs))

--Proyectores (ya están predefinidos).
{-
fTerm :: Interpretacion a -> (Nombre->[a]->a)
fTerm (I fT _) = fT

fPred :: Interpretacion a -> (Nombre->[a]->Bool)
fPred (I _ fP) = fP
-}

type Asignacion a = Nombre -> a

--Ejemplo para pruebas:
asignacion1::Asignacion Int
asignacion1 "X" = 0
asignacion1 "Y" = 1
asignacion1 "Z" = 2

evaluar::Asignacion a->(Nombre->[a]->a)->Termino->a
evaluar = error "Falta implementar."

--Ejemplo: evaluar asignacion1 (fTerm ejemploNat) $ Func "suma" [Func "suc" [Var "X"], Var "Y"]

--actualizarAsignacion :: Implementar y dar el tipo.

--Se usa una asignación de valores a las variables libres. Pueden usar recursión explícita, pero aclaren por qué no encaja bien en fold ni rec.
--Se puede hacer con fold cambiando el orden de los parámetros (flip), pero no es natural/sencillo. ¿por qué?
vale::Eq a =>Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
vale = error "Falta implementar."

-- Ejemplos (agreguen alguno con otra interpretación).

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"])
-- True

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X",Func "suc" [Var "X"]])
-- False

--vale ejemploNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--True

--vale ejemploNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--False
