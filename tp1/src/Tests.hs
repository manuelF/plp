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

-- suma(suc(x),y)
term1 = Func "suma" [Func "suc" [Var "X"], Var "Y"]
-- P(f(x)) -> Q(x)
form1 = Imp   (Pred "P" [Func "f" [Var "x"]])    (Pred "Q" [Var "x"])
-- ~(P(f(x))) v Q(x)
form2 = O (No (Pred "P" [Func "f" [Var "x"]]) )  (Pred "Q" [Var "x"])
-- V x E y (P(x)->q(x,y))
form3 = A "x" (E "y" (Imp (Pred "p" [Var "x"])(Pred "q" [Var "x", Var "y"])))
-- V x E y (~(p(x)) v q(x,y))
form4 = A "x" (E "y" (O (No (Pred "p" [Var "x"])) (Pred "q" [Var "x", Var "y"])))
-- ~(p(c()))
form5 = No (Pred "p" [Func "c" []])
-- p(c())
form6 = Pred "p" [Func "c" []]

allTests = test [ 
	"join" ~: testsJoin,
	"parentizar" ~: testsParentizar,
        "esLiteral" ~: testsEsLiteral,
        "eliminarImplicaciones" ~: testsEliminarImplicaciones,
        "aFNN" ~: testsAFNN,
        "FV" ~: testsFv,
        "evaluar" ~: testsEvaluar,
        "actualizarAsignacion" ~: testsActualizarAsignacion
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

testsEsLiteral = test [
       esLiteral form1 ~=? False,
       esLiteral form2 ~=? False,
       esLiteral form5 ~=? True,
       esLiteral form6 ~=? True
       ]

testsEliminarImplicaciones = test [
        formulaToString (eliminarImplicaciones  form1) ~=? formulaToString form2
        ]

testsAFNN = test [
         formulaToString (aFNN  form3) ~=? formulaToString form4
         ]

testsFv = test [
         fv form1 ~=? ["x"],
         fv form3 ~=? []
         ]


testsEvaluar = test [
        evaluar asignacion1 (fTerm ejemploNat)  term1 ~=? 2
        ]

testsActualizarAsignacion = test [
        (actualizarAsignacion "X" 99 asignacion1) "X" ~=? 99,
        (actualizarAsignacion "X" 99 asignacion1) "Y" ~=? 1
        ]
