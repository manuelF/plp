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


---- Ejercicio 1
{--
 Utiliza pattern matching para examinar el tipo algebraico del unico argumento
 y devuelve True en caso de ser un literal o False en caso contrario.
 esLiteral  Pred "p" [Func "c" []]      ⟶ True
 esLiteral  No (Pred "p" [Func "c" []]) ⟶ True
 esLiteral  Imp  (Pred "P" [Func "c" []])  (Pred "Q" [Func "d" []]) ⟶ False
--}
esLiteral :: Formula -> Bool
esLiteral (Pred _ _) = True
esLiteral (No _)     = True
esLiteral _          = False


---- Ejercicio 2
foldTermino ::   (Nombre -> b)         --casoVar 
            -> (Nombre -> [b] -> b)    --casoFunc
            -> Termino -> b            --termino
foldTermino casoVar casoFunc ter =
  case ter of
    Var n     -> casoVar n
    Func n ts -> casoFunc n (map rec ts)
  where rec = foldTermino casoVar casoFunc


---- Ejercicio 3
foldFormula ::   (Nombre -> [Termino] -> b) --casoPred 
            -> (b -> b)                     --casoNo
            -> (b -> b -> b)                --casoY
            -> (b -> b -> b)                --casoO
            -> (b -> b -> b)                --casoImp
            -> (Nombre -> b -> b)           --casoA
            -> (Nombre -> b -> b)           --casoE
            -> Formula                      --parametro
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


---- Ejercicio 4
--Esquema de recursión primitiva para fórmulas.
recFormula :: (Nombre -> [Termino] -> b)
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


---- Ejercicio 5
instance Show Termino where
  show = terminoToString
  
terminoToString :: Termino -> String
terminoToString = foldTermino mayusculirizar
                              (\n r -> if null r then n else parentizar n r)

mayusculirizar ::Nombre -> Nombre
mayusculirizar n = [toUpper l | l <- n]


join::[a]->[[a]]->[a]
join separador = foldr (\x res->if null res then x else x++separador++res) []

{- Toma un nombre de función y una lista de argumentos ya convertidos en String,
   y termina de armar la representación visual. -}
parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s++"("++(join "," res)++")"


---- Ejercicio 6
instance Show Formula where
-- Operadores lógicos: "¬","∧","∨","⊃","∀","∃"
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
                    

---- Ejercicio 7
eliminarImplicaciones :: Formula -> Formula
eliminarImplicaciones  = foldFormula Pred No Y O (\r1 r2 -> O (No r1) r2) A E


---- Ejercicio 8
aFNN::Formula->Formula
aFNN = foldFormula Pred negar Y O (\r1 r2 -> O (negar r1 ) r2) A E

negar :: Formula -> Formula
negar (No f) = f
negar f      = No f


---- Ejercicio 9
-- Tiene que haber una manera mejor de escribir esto
fv :: Formula -> [Nombre]
fv = foldFormula (\n ts -> concat (map obtenerVariables ts))
                 id
                 (\r1 r2 -> nub (r1 ++ r2))
                 (\r1 r2 -> nub (r1 ++ r2))
                 (\r1 r2 -> nub (r1 ++ r2))
                 (\n  r  -> filter (/=n) r)
                 (\n r   -> filter (/=n) r)  

obtenerVariables :: Termino -> [Nombre] 
obtenerVariables = foldTermino (:[]) (\n r -> concat r)

  
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

---- Ejercicio 10
--Ejemplo: evaluar asignacion1 (fTerm ejemploNat) $ Func "suma" [Func "suc" [Var "X"], Var "Y"]
evaluar::Asignacion a -> (Nombre -> [a] -> a) -> Termino -> a
evaluar asig ft  = foldTermino asig (\n r -> ft n r)


---- Ejercicio 11
-- *****¿En asignacion todas las variables estan en mayuscula?*****
actualizarAsignacion :: Nombre -> a -> Asignacion a -> Asignacion a
actualizarAsignacion nombre valor asig = \n -> if n == nombre
                                               then valor
                                               else asig n

---- Ejercicio 12
-- Se usa una asignación de valores a las variables libres. Pueden usar recursión
-- explícita, pero aclaren por qué no encaja bien en fold ni rec.
-- Se puede hacer con fold cambiando el orden de los parámetros (flip), pero no
-- es natural/sencillo. ¿por qué?

vale::Eq a =>Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
{--
vale inter dom asig f = foldFormula (\n ts -> fPred inter n ts)
                                    not
                                    (&&)
                                    (||)
                                    (-->)
                                    "Booom fold"
--}
vale inter dom asig (Pred n ts) = fPred inter n (map (evaluar asig (fTerm inter)) ts)
vale inter dom asig (No f)      = not (vale inter dom asig f)
vale inter dom asig (Y f1 f2)   = (vale inter dom asig f1) && (vale inter dom asig f2)
vale inter dom asig (Imp f1 f2) = (vale inter dom asig f1) --> (vale inter dom asig f2)
vale inter dom asig (A n f)     = and [vale inter dom (actualizarAsignacion n v asig) f | v <- dom]
vale inter dom asig (E n f)     = or  [vale inter dom (actualizarAsignacion n v asig) f | v <- dom]

(-->) :: Bool -> Bool -> Bool
(-->) x y = not x || y

-- Ejemplos (agreguen alguno con otra interpretación).

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"])
-- True

-- vale ejemploNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X",Func "suc" [Var "X"]])
-- False

--vale ejemploNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--True

--vale ejemploNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--False
