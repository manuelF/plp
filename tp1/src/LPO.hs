module LPO where

import Data.Char
import Data.List

type Nombre = String

data Termino = Var Nombre | Func Nombre [Termino]

data Formula = Pred Nombre [Termino] | No Formula | Y Formula Formula | O Formula Formula | Imp Formula Formula | A Nombre Formula | E Nombre Formula

--esLiteral :: Dar tipo e implementar.

esLiteral :: Formula -> Bool
esLiteral (Pred n t) = True
esLiteral (No p) = True
esLiteral _ = False

foldTermino :: (Nombre -> b) -> ( Nombre -> [b] -> b) -> Termino -> b
foldTermino f1 f2 = g
  where 
    g (Var n) = f1 n
    g (Func n t) = f2 n (map (foldTermino f1 f2) t)

foldFormula :: (Nombre -> [Termino] -> b) 
                -> (b -> b)               
                -> (b -> b -> b)         
                -> (b -> b -> b)         
                -> (b -> b -> b)         
                -> (Nombre -> b -> b)     
                -> (Nombre -> b -> b)          
                -> Formula
                -> b
foldFormula f1 f2 f3 f4 f5 f6 f7 = g
  where 
    g (Pred n t) = f1 n t 
    g (No x) = f2 (g a)
    g (Y a b) = f3 (g a) (g b)
    g (O a b) = f4 (g a) (g b)
    g (Imp a b) = f5 (g a) (g b)
    g (A n a) = f6 n (g a)
    g (E n a) = f7 n (g a)

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
    g (No a) = f2 f (g a) 
    g (Y a b) = f3 a b (g a) (g b) 
    g (O a b) = f4 a b (g a) (g b)
    g (Imp a b) = f5 a b (g a) (g b)
    g (A n a) = f6 b n (g a)
    g (E n a) = f7 b n (g a) 

instance Show Termino where
  show = error "Falta implementar."
				      
join::[a]->[[a]]->[a]
join separador = foldr (\x res->if null res then x else x++separador++res) []

{- Toma un nombre de función y una lista de argumentos ya convertidos en String, y termina de armar la representación visual. -}
parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s++"("++(join "," res)++")"

instance Show Formula where
-- Operadores lógicos: "¬","∧","∨","⊃","∀","∃"
    show = error "Falta implementar."

--Ejemplo: A "x" (Imp (Pred "p" [Var "x"]) (Pred "p" [Var "x"])) se ve como ∀X.((P(X)⊃P(X)))

--eliminarImplicaciones :: Dar tipo e implementar.

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
