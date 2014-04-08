import LPO
import HUnit

{-
	Para correr los tests deben cargar en hugs el módulo Tests
	y evaluar la expresión "main".

	Se incluye también la definición de (~~?) y (~~), que pueden usar
	para comparar listas sin importar el orden de sus elementos.
-}

main = runTestTT allTests

(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

(~~) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Bool
expected ~~ actual = (sort expected) == (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)


{-
	Definición de los casos de test.
	
	No olviden agregar sus tests al listado allTests para que se ejecuten.
-}
 
allTests = test [ 
	"join" ~: testsJoin,
	"parentizar" ~: testsParentizar,
        "eliminarImplicaciones" ~: testsEliminarImplicaciones,
        "aFNN" ~: testsAFNN
	]

testsJoin = test [
	join "," [] ~=? "",
	join "," ["x"] ~=? "x",
	join "," ["x", "y"] ~=? "x,y"
	]

testsParentizar = test [
	parentizar "f" [] ~=? "f",
	parentizar "f" ["x"] ~=? "f(x)",
	parentizar "f" ["x", "y"] ~=? "f(x,y)"
	]


form1 = Imp   (Pred "P" [Func "f" [Var "x"]])    (Pred "Q" [Var "x"])
form2 = O (No (Pred "P" [Func "f" [Var "x"]]) )  (Pred "Q" [Var "x"])
form3 = A "x" (E "y" (Imp (Pred "p" [Var "x"])(Pred "q" [Var "x", Var "y"])))
form4 = A "x" (E "y" (O (No (Pred "p" [Var "x"])) (Pred "q" [Var "x", Var "y"])))

testsEliminarImplicaciones = test [
        formulaToString (eliminarImplicaciones  form1) ~=? formulaToString form2
        ]

testsAFNN = test [
         formulaToString (aFNN  form3) ~=? formulaToString form4
         ]
            
