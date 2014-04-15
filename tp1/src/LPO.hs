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

operadoresLogicos = ["¬","∧","∨","⊃","∀","∃"]

--------------------
-- Ejercicio 1
--------------------
-- Utiliza pattern matching para examinar el tipo algebraico del unico argumento
-- y devuelve True en caso de ser un literal o False en caso contrario.
-- Ejemplos:
--  esLiteral  Pred "p" [Func "c" []]      -> True
--  esLiteral  No (Pred "p" [Func "c" []]) -> True
--  esLiteral  Imp  (Pred "P" [Func "c" []])  (Pred "Q" [Func "d" []]) ->  False
esLiteral :: Formula -> Bool
esLiteral (Pred _ _)      = True
esLiteral (No (Pred _ _)) = True
esLiteral _               = False

--------------------
-- Ejercicio 2
--------------------
-- Implementacion de recursion estructural sobre los terminos.
-- Si es una variable, se aplica la primer funcion sobre el nombre. Si es
-- una Func, se aplica la segunda funcion sobre el nombre y el resultado de
-- foldear sobre la lista de terminos.
foldTermino :: (Nombre -> b)           -- Caso Var
            -> (Nombre -> [b] -> b)    -- Caso Func
            -> Termino -> b            -- Termino
foldTermino casoVar casoFunc ter =
  case ter of
    Var n     -> casoVar n
    Func n ts -> casoFunc n (map (foldTermino casoVar casoFunc) ts)

--------------------
-- Ejercicio 3
--------------------
-- Implementacion de recursion estructural sobre las formulas.
-- Aplica la funcion correspondiente dada la estructura del parametro.
-- Antes de aplicar la funcion, primero foldea sobre los parametros.
foldFormula :: (Nombre -> [Termino] -> b)   -- Caso Pred
            -> (b -> b)                     -- Caso No
            -> (b -> b -> b)                -- Caso Y
            -> (b -> b -> b)                -- Caso O
            -> (b -> b -> b)                -- Caso Imp
            -> (Nombre -> b -> b)           -- Caso A
            -> (Nombre -> b -> b)           -- Caso E
            -> Formula                      -- parametro
            -> b
foldFormula casoPred casoNo casoY casoO casoImp casoA casoE f =
  case f of
    Pred n ts -> casoPred n ts
    No a      -> casoNo (rec a)
    Y a b     -> casoY (rec a) (rec b)
    O a b     -> casoO (rec a) (rec b)
    Imp a b   -> casoImp (rec a) (rec b)
    A n a     -> casoA n (rec a)
    E n a     -> casoE n (rec a)
  where rec = foldFormula casoPred casoNo casoY casoO casoImp casoA casoE

--------------------
-- Ejercicio 4
--------------------
--Esquema de recursión primitiva para fórmulas.
recFormula :: (Nombre -> [Termino] -> b)          -- Caso Pred
           -> (Formula -> b -> b)                 -- Caso No
           -> (Formula -> Formula -> b -> b -> b) -- Caso Y
           -> (Formula -> Formula -> b -> b -> b) -- Caso O
           -> (Formula -> Formula -> b -> b -> b) -- Caso Imp
           -> (Formula -> Nombre -> b -> b)       -- Caso A
           -> (Formula -> Nombre -> b -> b)       -- Caso E
           -> Formula
           -> b
recFormula casoPred casoNo casoY casoO casoImp casoA casoE = g
  where
    g (Pred n t)  = casoPred n t
    g (No a)      = casoNo a (g a)
    g (Y a b)     = casoY a b (g a) (g b)
    g (O a b)     = casoO a b (g a) (g b)
    g (Imp a b)   = casoImp a b (g a) (g b)
    g (A n a)     = casoA a n (g a)
    g (E n a)     = casoE a n (g a)

--------------------
-- Ejercicio 5
--------------------
-- Muestra un termino en formato string. Para eso usa la recursion sobre el fold
-- especifico para Terminos, pasando a mayusculas las variables y poniendo separadores
-- de parentesis para las funciones.
instance Show Termino where
  show = foldTermino mayusculirizar parentizar

mayusculirizar :: Nombre -> Nombre
mayusculirizar = map toUpper

-- Dada un lista, devulve una funcion que toma una lista de listas y que devuelve
-- una lista de esos elementos separados por los separadores tomados como parametro.
join :: [a] -> [[a]] -> [a]
join separador = foldr (\x res -> if null res then x else x++separador++res) []

-- Toma un nombre de función y una lista de argumentos ya convertidos en String,
-- y termina de armar la representación visual.
parentizar :: Nombre -> [String] -> String
parentizar s res = if null res then s else s ++ "(" ++ (join "," res) ++ ")"

--------------------
-- Ejercicio 6
--------------------
-- Toma una formula y foldea reemplazando cada constructor con un comportamiento
-- especifico sobre como imprimir en ese caso.
instance Show Formula where
    show =  foldFormula
            (\n ts  -> parentizar (mayusculirizar n) (map show ts))
            (\r     -> if esReprDeLiteral r then  "¬" ++ r else "¬(" ++ r ++ ")")
            (\r1 r2 -> r1 ++ "∧" ++ r2)
            (\r1 r2 -> r1 ++ "∨" ++ r2)
            (\r1 r2 -> r1 ++ "⊃" ++ r2)
            (\n r   -> "∀" ++ (mayusculirizar n) ++ ".("  ++ r ++ ")")
            (\n r   -> "∃" ++ (mayusculirizar n) ++ ".(" ++  r ++ ")")

-- Funcion auxiliar que verifica si en una representacion en string de una formula
-- hay algun operador logico. Chequeo rapido de si es un literal o no.
esReprDeLiteral :: String -> Bool
esReprDeLiteral s = not (any (\x -> isInfixOf x s) operadoresLogicos)

--------------------
-- Ejercicio 7
--------------------
-- Toma una formula y foldea como si fuera identidad pero reemplazando las
-- implicaciones con ~A o B.
eliminarImplicaciones :: Formula -> Formula
eliminarImplicaciones  = foldFormula Pred No Y O (\r1 r2 -> O (No r1) r2) A E

--------------------
-- Ejercicio 8
--------------------
-- Toma una formula y foldea negando las subformulas y reemplazando las implicaciones
-- con ~A o B. La funcion usada en el fold para el caso del constructor 'No'
-- es 'negar', que implementa la logica de como meter las negaciones adentro.
aFNN::Formula->Formula
aFNN = foldFormula Pred negar Y O (\r1 r2 -> O (negar r1) r2) A E

-- Funcion auxiliar que niega los terminos internos de una formula.
negar :: Formula -> Formula
negar = recFormula
        (\n ts -> No (Pred n ts))         -- Caso Pred: Negar los predicados.
        (\f r -> f)                       -- Caso No: Quitar la negacion.
        (\f1 f2 r1 r2 -> O r1 r2)         -- Caso Y: Negar Y -> O
        (\f1 f2 r1 r2 -> Y r1 r2)         -- Caso O: Negar O -> Y
        (\f1 f2 r1 r2 -> No (Imp r1 r2))  -- Caso Imp: Negar la implicacion
        (\f n r -> E n r)                 -- Caso A: Negar A -> E
        (\f n r -> A n r)                 -- Caso E: Negar E -> A

--------------------
-- Ejercicio 9
--------------------
-- Toma una formula y devuelve la lista de nombres de todas las variables
-- no ligadas.
fv :: Formula -> [Nombre]
fv = foldFormula (\n ts -> concat (map obtenerVariables ts)) -- Caso Pred: Devuelve todas las variable.
                 id                                          -- Caso No: Si es una negacion de formula, las mismas variables.
                 (\r1 r2 -> nub (r1 ++ r2))                  -- Caso Y: Devolvemos las desduplicaciones de ambas subformulas.
                 (\r1 r2 -> nub (r1 ++ r2))                  -- Caso O: Idem ^^
                 (\r1 r2 -> nub (r1 ++ r2))                  -- Caso Imp: Idem ^^
                 (\n  r  -> filter (/=n) r)                  -- Caso A: Devolvemos las variables de las subformulas que sean
                                                             --     distintas a la ligada variable ligada aca.
                 (\n r   -> filter (/=n) r)                  -- Caso E: Idem ^^

-- Proyeccion de los nombres de las variables de un termino en una lista.
obtenerVariables :: Termino -> [Nombre]
obtenerVariables = foldTermino (:[]) (\n r -> concat r)


--Interpretación en un dominio a. Una función para términos y otra para predicados.
--Basta con que las funciones estén bien definidas para su dominio esperado.
data Interpretacion a = I {fTerm :: (Nombre->[a]->a), fPred :: (Nombre->[a]->Bool)}

--Ejemplo para pruebas:
interpretacionNat::Interpretacion Int
interpretacionNat = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "0" = const 0
                    | nombreF == "suc" = \xs -> head xs + 1
                    | nombreF == "suma" = sum
  fPredicados nombreP | nombreP == "esCero" = \xs -> head xs == 0
                      | nombreP == "esPar" = \xs -> mod (head xs) 2 == 0
                      | nombreP == "mayor" = \xs -> (head xs) > (head (tail xs))
                      | nombreP == "menor" = \xs -> (head xs) < (head (tail xs))

interpretacionZ::Interpretacion Int
interpretacionZ = I fTerminos fPredicados where
  fTerminos nombreF | nombreF == "0" = const 0
                    | nombreF == "suc" = \xs -> head xs + 1
                    | nombreF == "suma" = sum
                    | nombreF == "invertir" = \xs -> head xs * (-1) 
                    | nombreF == "producto" = product
  fPredicados nombreP | nombreP == "esCero" = \xs -> head xs == 0
                      | nombreP == "esPositivo" = \xs -> head xs > 0
                      | nombreP == "esNegativo" = \xs -> head xs < 0

--Proyectores (ya están predefinidos).
{-
fTerm :: Interpretacion a -> (Nombre->[a]->a)
fTerm (I fT _) = fT

fPred :: Interpretacion a -> (Nombre->[a]->Bool)
fPred (I _ fP) = fP
-}

type Asignacion a = Nombre -> a

--------------------
-- Ejercicio 10
--------------------
-- Devuelve un valor para una formula dada una asignacion de sus variables.
-- Ejemplo: evaluar asignacion1 (fTerm interpretacionNat) $ Func "suma" [Func "suc" [Var "X"], Var "Y"]
evaluar::Asignacion a -> (Nombre -> [a] -> a) -> Termino -> a
evaluar = foldTermino

--------------------
-- Ejercicio 11
--------------------
-- Dada un nombre de una variable, un valor y una asignacion, devuelve una funcion
-- que actualiza el valor de la asignacion al pasado por parametro si el nombre coincide.
actualizarAsignacion :: Nombre -> a -> Asignacion a -> Asignacion a
actualizarAsignacion nombre valor asig = \n -> if n == nombre
                                               then valor
                                               else asig n

--------------------
-- Ejercicio 12
--------------------
-- Se usa una asignación de valores a las variables libres. Pueden usar recursión
-- explícita, pero aclaren por qué no encaja bien en fold ni rec.
-- Se puede hacer con fold cambiando el orden de los parámetros (flip), pero no
-- es natural/sencillo. ¿por qué?

-- Ejemplos (agreguen alguno con otra interpretación).

-- vale interpretacionNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"])
-- True

-- vale interpretacionNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X",Func "suc" [Var "X"]])
-- False

--vale interpretacionNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--True

--vale interpretacionNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"]))
--False


vale::Eq a => Interpretacion a -> [a] -> Asignacion a -> Formula -> Bool
{--
vale inter dom asig f = foldFormula (\n ts -> fPred inter n ts)
                                    not
                                    (&&)
                                    (||)
                                    (-->)
                                    "Booom fold"
--}
--Caso Pred: La valuacion de un predicado es la valuacion de sus terminos dadas las asignaciones.
vale inter dom asig (Pred n ts) = fPred inter n (map (evaluar asig (fTerm inter)) ts)
--Caso No: La valuacion de una negacion es la negacion de la valuacion.
vale inter dom asig (No f)      = not (vale inter dom asig f)
--Caso Y: La valuacion de una conjuncion es la conjuncion de las valuaciones.
vale inter dom asig (Y f1 f2)   = (vale inter dom asig f1) && (vale inter dom asig f2)
--Caso Imp: La valuacion de una implicacion es la implicacion de las valuaciones.
vale inter dom asig (Imp f1 f2) = (vale inter dom asig f1) --> (vale inter dom asig f2)
--Caso A: La valuacion del cuantificar universal es la conjuncion de todas las valuaciones de
--la formula con la variable ligada.
vale inter dom asig (A n f)     = and [vale inter dom (actualizarAsignacion n v asig) f | v <- dom]
--Caso E: La valuacion del cuantificador existencial es la disyuncion de todas las valuaciones
--de la formula con la variable ligada.
vale inter dom asig (E n f)     = or  [vale inter dom (actualizarAsignacion n v asig) f | v <- dom]

-- Auxiliar: Operador implicacion
(-->) :: Bool -> Bool -> Bool
(-->) x y = not x || y

