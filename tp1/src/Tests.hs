import LPO
import HUnit
import Formulas

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
  "esLiteral" ~: testsEsLiteral,
  "Formula.show" ~: testsShowFormula,
  "eliminarImplicaciones" ~: testsEliminarImplicaciones,
  "aFNN" ~: testsAFNN,
  "FV" ~: testsFv,
  "evaluar" ~: testsEvaluar,
  "actualizarAsignacion" ~: testsActualizarAsignacion,
  "valer" ~: testVale
	]

-- Tests ejercicio 1
testsEsLiteral = test [
       esLiteral form2 ~=? False,
       esLiteral form3 ~=? False,
       esLiteral p ~=? True,
       esLiteral noQ ~=? True,
       esLiteral (No noQ) ~=? True
       ]


-- Tests ejercicio 5 y 6
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

testsShowFormula = test [
        show form1   ~=? "¬¬P(X)",
        show form2   ~=? "P(X)∧Q(X,Y)",
        show form3   ~=? "¬P(X)∧Q(X,Y)",
        show form4   ~=? "P(X)∧¬Q(X,Y)",
        show form5   ~=? "¬P(X)∧¬Q(X,Y)",
        show form6   ~=? "¬(P(X)∧Q(X,Y))",
        show form7   ~=? "¬(¬P(X)∧Q(X,Y))",
        show form8   ~=? "¬(P(X)∧¬Q(X,Y))",
        show form9   ~=? "¬(¬P(X)∧¬Q(X,Y))",
        show form10  ~=? "P(X)∨Q(X,Y)",
        show form11  ~=? "¬P(X)∨Q(X,Y)",
        show form12  ~=? "P(X)∨¬Q(X,Y)",
        show form13  ~=? "¬P(X)∨¬Q(X,Y)",
        show form14  ~=? "¬(P(X)∨Q(X,Y))",
        show form15  ~=? "¬(¬P(X)∨Q(X,Y))",
        show form16  ~=? "¬(P(X)∨¬Q(X,Y))",
        show form17  ~=? "¬(¬P(X)∨¬Q(X,Y))",
        show form18  ~=? "P(X)⊃Q(X,Y)",
        show form19  ~=? "¬P(X)⊃Q(X,Y)",
        show form20  ~=? "P(X)⊃¬Q(X,Y)",
        show form21  ~=? "¬P(X)⊃¬Q(X,Y)",
        show form22  ~=? "¬(P(X)⊃Q(X,Y))",
        show form23  ~=? "¬(¬P(X)⊃Q(X,Y))",
        show form24  ~=? "¬(P(X)⊃¬Q(X,Y))",
        show form25  ~=? "¬(¬P(X)⊃¬Q(X,Y))",
        show form26  ~=? "∀X.(¬¬P(X))",
        show form27  ~=? "∀X.(P(X)∧Q(X,Y))",
        show form28  ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show form29  ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show form30  ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show form31  ~=? "∀X.(¬(P(X)∧Q(X,Y)))",
        show form32  ~=? "∀X.(¬(¬P(X)∧Q(X,Y)))",
        show form33  ~=? "∀X.(¬(P(X)∧¬Q(X,Y)))",
        show form34  ~=? "∀X.(¬(¬P(X)∧¬Q(X,Y)))",
        show form35  ~=? "∀X.(P(X)∨Q(X,Y))",
        show form36  ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show form37  ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show form38  ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show form39  ~=? "∀X.(¬(P(X)∨Q(X,Y)))",
        show form40  ~=? "∀X.(¬(¬P(X)∨Q(X,Y)))",
        show form41  ~=? "∀X.(¬(P(X)∨¬Q(X,Y)))",
        show form42  ~=? "∀X.(¬(¬P(X)∨¬Q(X,Y)))",
        show form43  ~=? "∀X.(P(X)⊃Q(X,Y))",
        show form44  ~=? "∀X.(¬P(X)⊃Q(X,Y))",
        show form45  ~=? "∀X.(P(X)⊃¬Q(X,Y))",
        show form46  ~=? "∀X.(¬P(X)⊃¬Q(X,Y))",
        show form47  ~=? "∀X.(¬(P(X)⊃Q(X,Y)))",
        show form48  ~=? "∀X.(¬(¬P(X)⊃Q(X,Y)))",
        show form49  ~=? "∀X.(¬(P(X)⊃¬Q(X,Y)))",
        show form50  ~=? "∀X.(¬(¬P(X)⊃¬Q(X,Y)))",
        show form51  ~=? "¬(∀X.(¬¬P(X)))",
        show form52  ~=? "¬(∀X.(P(X)∧Q(X,Y)))",
        show form53  ~=? "¬(∀X.(¬P(X)∧Q(X,Y)))",
        show form54  ~=? "¬(∀X.(P(X)∧¬Q(X,Y)))",
        show form55  ~=? "¬(∀X.(¬P(X)∧¬Q(X,Y)))",
        show form56  ~=? "¬(∀X.(¬(P(X)∧Q(X,Y))))",
        show form57  ~=? "¬(∀X.(¬(¬P(X)∧Q(X,Y))))",
        show form58  ~=? "¬(∀X.(¬(P(X)∧¬Q(X,Y))))",
        show form59  ~=? "¬(∀X.(¬(¬P(X)∧¬Q(X,Y))))",
        show form60  ~=? "¬(∀X.(P(X)∨Q(X,Y)))",
        show form61  ~=? "¬(∀X.(¬P(X)∨Q(X,Y)))",
        show form62  ~=? "¬(∀X.(P(X)∨¬Q(X,Y)))",
        show form63  ~=? "¬(∀X.(¬P(X)∨¬Q(X,Y)))",
        show form64  ~=? "¬(∀X.(¬(P(X)∨Q(X,Y))))",
        show form65  ~=? "¬(∀X.(¬(¬P(X)∨Q(X,Y))))",
        show form66  ~=? "¬(∀X.(¬(P(X)∨¬Q(X,Y))))",
        show form67  ~=? "¬(∀X.(¬(¬P(X)∨¬Q(X,Y))))",
        show form68  ~=? "¬(∀X.(P(X)⊃Q(X,Y)))",
        show form69  ~=? "¬(∀X.(¬P(X)⊃Q(X,Y)))",
        show form70  ~=? "¬(∀X.(P(X)⊃¬Q(X,Y)))",
        show form71  ~=? "¬(∀X.(¬P(X)⊃¬Q(X,Y)))",
        show form72  ~=? "¬(∀X.(¬(P(X)⊃Q(X,Y))))",
        show form73  ~=? "¬(∀X.(¬(¬P(X)⊃Q(X,Y))))",
        show form74  ~=? "¬(∀X.(¬(P(X)⊃¬Q(X,Y))))",
        show form75  ~=? "¬(∀X.(¬(¬P(X)⊃¬Q(X,Y))))",
        show form76  ~=? "∃X.(¬¬P(X))",
        show form77  ~=? "∃X.(P(X)∧Q(X,Y))",
        show form78  ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show form79  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show form80  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show form81  ~=? "∃X.(¬(P(X)∧Q(X,Y)))",
        show form82  ~=? "∃X.(¬(¬P(X)∧Q(X,Y)))",
        show form83  ~=? "∃X.(¬(P(X)∧¬Q(X,Y)))",
        show form84  ~=? "∃X.(¬(¬P(X)∧¬Q(X,Y)))",
        show form85  ~=? "∃X.(P(X)∨Q(X,Y))",
        show form86  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show form87  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show form88  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show form89  ~=? "∃X.(¬(P(X)∨Q(X,Y)))",
        show form90  ~=? "∃X.(¬(¬P(X)∨Q(X,Y)))",
        show form91  ~=? "∃X.(¬(P(X)∨¬Q(X,Y)))",
        show form92  ~=? "∃X.(¬(¬P(X)∨¬Q(X,Y)))",
        show form93  ~=? "∃X.(P(X)⊃Q(X,Y))",
        show form94  ~=? "∃X.(¬P(X)⊃Q(X,Y))",
        show form95  ~=? "∃X.(P(X)⊃¬Q(X,Y))",
        show form96  ~=? "∃X.(¬P(X)⊃¬Q(X,Y))",
        show form97  ~=? "∃X.(¬(P(X)⊃Q(X,Y)))",
        show form98  ~=? "∃X.(¬(¬P(X)⊃Q(X,Y)))",
        show form99  ~=? "∃X.(¬(P(X)⊃¬Q(X,Y)))",
        show form100 ~=? "∃X.(¬(¬P(X)⊃¬Q(X,Y)))",
        show form101 ~=? "¬(∃X.(¬¬P(X)))",
        show form102 ~=? "¬(∃X.(P(X)∧Q(X,Y)))",
        show form103 ~=? "¬(∃X.(¬P(X)∧Q(X,Y)))",
        show form104 ~=? "¬(∃X.(P(X)∧¬Q(X,Y)))",
        show form105 ~=? "¬(∃X.(¬P(X)∧¬Q(X,Y)))",
        show form106 ~=? "¬(∃X.(¬(P(X)∧Q(X,Y))))",
        show form107 ~=? "¬(∃X.(¬(¬P(X)∧Q(X,Y))))",
        show form108 ~=? "¬(∃X.(¬(P(X)∧¬Q(X,Y))))",
        show form109 ~=? "¬(∃X.(¬(¬P(X)∧¬Q(X,Y))))",
        show form110 ~=? "¬(∃X.(P(X)∨Q(X,Y)))",
        show form111 ~=? "¬(∃X.(¬P(X)∨Q(X,Y)))",
        show form112 ~=? "¬(∃X.(P(X)∨¬Q(X,Y)))",
        show form113 ~=? "¬(∃X.(¬P(X)∨¬Q(X,Y)))",
        show form114 ~=? "¬(∃X.(¬(P(X)∨Q(X,Y))))",
        show form115 ~=? "¬(∃X.(¬(¬P(X)∨Q(X,Y))))",
        show form116 ~=? "¬(∃X.(¬(P(X)∨¬Q(X,Y))))",
        show form117 ~=? "¬(∃X.(¬(¬P(X)∨¬Q(X,Y))))",
        show form118 ~=? "¬(∃X.(P(X)⊃Q(X,Y)))",
        show form119 ~=? "¬(∃X.(¬P(X)⊃Q(X,Y)))",
        show form120 ~=? "¬(∃X.(P(X)⊃¬Q(X,Y)))",
        show form121 ~=? "¬(∃X.(¬P(X)⊃¬Q(X,Y)))",
        show form122 ~=? "¬(∃X.(¬(P(X)⊃Q(X,Y))))",
        show form123 ~=? "¬(∃X.(¬(¬P(X)⊃Q(X,Y))))",
        show form124 ~=? "¬(∃X.(¬(P(X)⊃¬Q(X,Y))))",
        show form125 ~=? "¬(∃X.(¬(¬P(X)⊃¬Q(X,Y))))",
        show form126 ~=? "∀Y.(∃X.(P(X)∧Q(X,Y)))",
        show form127 ~=? "∀Y.(∃X.(P(X)∨Q(X,Y)))",
        show form128 ~=? "∀Y.(∃X.(P(X)⊃Q(X,Y)))",
        show form129 ~=? "∀Y.(∀X.(P(X)∧Q(X,Y)))",
        show form130 ~=? "∀Y.(¬(∃X.(P(X)∧Q(X,Y))))",
        show form131 ~=? "∀Y.(¬(∃X.(P(X)∨Q(X,Y))))",
        show form132 ~=? "∀Y.(¬(∃X.(P(X)⊃Q(X,Y))))",
        show form133 ~=? "∀Y.(¬(∀X.(P(X)∧Q(X,Y))))",
        show form134 ~=? "∃Y.(∀X.(P(X)∧Q(X,Y)))",
        show form135 ~=? "∃Y.(∀X.(P(X)∨Q(X,Y)))",
        show form136 ~=? "∃Y.(∀X.(P(X)⊃Q(X,Y)))",
        show form137 ~=? "∃Y.(∃X.(P(X)∧Q(X,Y)))",
        show form138 ~=? "∃Y.(¬(∀X.(P(X)∧Q(X,Y))))",
        show form139 ~=? "∃Y.(¬(∀X.(P(X)∨Q(X,Y))))",
        show form140 ~=? "∃Y.(¬(∀X.(P(X)⊃Q(X,Y))))",
        show form141 ~=? "∃Y.(¬(∃X.(P(X)∧Q(X,Y))))"
  ]
-- Tests ejercicio 7
testsEliminarImplicaciones = test [
        show (eliminarImplicaciones form18)  ~=? "¬P(X)∨Q(X,Y)",
        show (eliminarImplicaciones form19)  ~=? "¬¬P(X)∨Q(X,Y)",
        show (eliminarImplicaciones form20)  ~=? "¬P(X)∨¬Q(X,Y)",
        show (eliminarImplicaciones form21)  ~=? "¬¬P(X)∨¬Q(X,Y)",
        show (eliminarImplicaciones form22)  ~=? "¬(¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form23)  ~=? "¬(¬¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form24)  ~=? "¬(¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form25)  ~=? "¬(¬¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form43)  ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form44)  ~=? "∀X.(¬¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form45)  ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form46)  ~=? "∀X.(¬¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form47)  ~=? "∀X.(¬(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form48)  ~=? "∀X.(¬(¬¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form49)  ~=? "∀X.(¬(¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form50)  ~=? "∀X.(¬(¬¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form68)  ~=? "¬(∀X.(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form69)  ~=? "¬(∀X.(¬¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form70)  ~=? "¬(∀X.(¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form71)  ~=? "¬(∀X.(¬¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form72)  ~=? "¬(∀X.(¬(¬P(X)∨Q(X,Y))))",
        show (eliminarImplicaciones form73)  ~=? "¬(∀X.(¬(¬¬P(X)∨Q(X,Y))))",
        show (eliminarImplicaciones form74)  ~=? "¬(∀X.(¬(¬P(X)∨¬Q(X,Y))))",
        show (eliminarImplicaciones form75)  ~=? "¬(∀X.(¬(¬¬P(X)∨¬Q(X,Y))))",
        show (eliminarImplicaciones form93)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form94)  ~=? "∃X.(¬¬P(X)∨Q(X,Y))",
        show (eliminarImplicaciones form95)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form96)  ~=? "∃X.(¬¬P(X)∨¬Q(X,Y))",
        show (eliminarImplicaciones form97)  ~=? "∃X.(¬(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form98)  ~=? "∃X.(¬(¬¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form99)  ~=? "∃X.(¬(¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form100) ~=? "∃X.(¬(¬¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form118) ~=? "¬(∃X.(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form119) ~=? "¬(∃X.(¬¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form120) ~=? "¬(∃X.(¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form121) ~=? "¬(∃X.(¬¬P(X)∨¬Q(X,Y)))",
        show (eliminarImplicaciones form122) ~=? "¬(∃X.(¬(¬P(X)∨Q(X,Y))))",
        show (eliminarImplicaciones form123) ~=? "¬(∃X.(¬(¬¬P(X)∨Q(X,Y))))",
        show (eliminarImplicaciones form124) ~=? "¬(∃X.(¬(¬P(X)∨¬Q(X,Y))))",
        show (eliminarImplicaciones form125) ~=? "¬(∃X.(¬(¬¬P(X)∨¬Q(X,Y))))",
        show (eliminarImplicaciones form128) ~=? "∀Y.(∃X.(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form132) ~=? "∀Y.(¬(∃X.(¬P(X)∨Q(X,Y))))",
        show (eliminarImplicaciones form136) ~=? "∃Y.(∀X.(¬P(X)∨Q(X,Y)))",
        show (eliminarImplicaciones form140) ~=? "∃Y.(¬(∀X.(¬P(X)∨Q(X,Y))))"
        ]

-- Tests ejercicio 8
testsAFNN = test [
        show (aFNN  form1)   ~=? "P(X)",
        show (aFNN  form6)   ~=? "¬P(X)∨¬Q(X,Y)",
        show (aFNN  form7)   ~=? "P(X)∨¬Q(X,Y)",
        show (aFNN  form8)   ~=? "¬P(X)∨Q(X,Y)",
        show (aFNN  form9)   ~=? "P(X)∨Q(X,Y)", 
        show (aFNN  form14)  ~=? "¬P(X)∧¬Q(X,Y)",
        show (aFNN  form15)  ~=? "P(X)∧¬Q(X,Y)",
        show (aFNN  form16)  ~=? "¬P(X)∧Q(X,Y)",
        show (aFNN  form17)  ~=? "P(X)∧Q(X,Y)", 
        show (aFNN  form18)  ~=? "¬P(X)∨Q(X,Y)",
        show (aFNN  form19)  ~=? "P(X)∨Q(X,Y)", 
        show (aFNN  form20)  ~=? "¬P(X)∨¬Q(X,Y)",
        show (aFNN  form21)  ~=? "P(X)∨¬Q(X,Y)",
        show (aFNN  form22)  ~=? "P(X)∧¬Q(X,Y)",
        show (aFNN  form23)  ~=? "¬P(X)∧¬Q(X,Y)",
        show (aFNN  form24)  ~=? "P(X)∧Q(X,Y)",
        show (aFNN  form25)  ~=? "¬P(X)∧Q(X,Y)", 
        show (aFNN  form26)  ~=? "∀X.(P(X))",
        show (aFNN  form31)  ~=? "∀X.(¬P(X)∨¬Q(X,Y))",  
        show (aFNN  form32)  ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form33)  ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form34)  ~=? "∀X.(P(X)∨Q(X,Y))",
        show (aFNN  form39)  ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form40)  ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form41)  ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form42)  ~=? "∀X.(P(X)∧Q(X,Y))",
        show (aFNN  form43)  ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form44)  ~=? "∀X.(P(X)∨Q(X,Y))",
        show (aFNN  form45)  ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form46)  ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form47)  ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form48)  ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form49)  ~=? "∀X.(P(X)∧Q(X,Y))", 
        show (aFNN  form50)  ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form51)  ~=? "∃X.(¬P(X))",
        show (aFNN  form52)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form53)  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form54)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form55)  ~=? "∃X.(P(X)∨Q(X,Y))",
        show (aFNN  form56)  ~=? "∃X.(P(X)∧Q(X,Y))",
        show (aFNN  form57)  ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form58)  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form59)  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form60)  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form61)  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form62)  ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form63)  ~=? "∃X.(P(X)∧Q(X,Y))",
        show (aFNN  form64)  ~=? "∃X.(P(X)∨Q(X,Y))",
        show (aFNN  form65)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form66)  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form67)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form68)  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form69)  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form70)  ~=? "∃X.(P(X)∧Q(X,Y))",
        show (aFNN  form71)  ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form72)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form73)  ~=? "∃X.(P(X)∨Q(X,Y))",
        show (aFNN  form74)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form75)  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form76)  ~=? "∃X.(P(X))",
        show (aFNN  form81)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form82)  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form83)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form84)  ~=? "∃X.(P(X)∨Q(X,Y))",
        show (aFNN  form89)  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form90)  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form91)  ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form92)  ~=? "∃X.(P(X)∧Q(X,Y))",
        show (aFNN  form93)  ~=? "∃X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form94)  ~=? "∃X.(P(X)∨Q(X,Y))",
        show (aFNN  form95)  ~=? "∃X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form96)  ~=? "∃X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form97)  ~=? "∃X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form98)  ~=? "∃X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form99)  ~=? "∃X.(P(X)∧Q(X,Y))",
        show (aFNN  form100) ~=? "∃X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form101) ~=? "∀X.(¬P(X))",
        show (aFNN  form102) ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form103) ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form104) ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form105) ~=? "∀X.(P(X)∨Q(X,Y))",
        show (aFNN  form106) ~=? "∀X.(P(X)∧Q(X,Y))",
        show (aFNN  form107) ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form108) ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form109) ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form110) ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form111) ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form112) ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form113) ~=? "∀X.(P(X)∧Q(X,Y))",
        show (aFNN  form114) ~=? "∀X.(P(X)∨Q(X,Y))",
        show (aFNN  form115) ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form116) ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form117) ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form118) ~=? "∀X.(P(X)∧¬Q(X,Y))",
        show (aFNN  form119) ~=? "∀X.(¬P(X)∧¬Q(X,Y))",
        show (aFNN  form120) ~=? "∀X.(P(X)∧Q(X,Y))",
        show (aFNN  form121) ~=? "∀X.(¬P(X)∧Q(X,Y))",
        show (aFNN  form122) ~=? "∀X.(¬P(X)∨Q(X,Y))",
        show (aFNN  form123) ~=? "∀X.(P(X)∨Q(X,Y))",
        show (aFNN  form124) ~=? "∀X.(¬P(X)∨¬Q(X,Y))",
        show (aFNN  form125) ~=? "∀X.(P(X)∨¬Q(X,Y))",
        show (aFNN  form128) ~=? "∀Y.(∃X.(¬P(X)∨Q(X,Y)))",
        show (aFNN  form130) ~=? "∀Y.(∀X.(¬P(X)∨¬Q(X,Y)))",
        show (aFNN  form131) ~=? "∀Y.(∀X.(¬P(X)∧¬Q(X,Y)))",
        show (aFNN  form132) ~=? "∀Y.(∀X.(P(X)∧¬Q(X,Y)))",
        show (aFNN  form133) ~=? "∀Y.(∃X.(¬P(X)∨¬Q(X,Y)))",
        show (aFNN  form136) ~=? "∃Y.(∀X.(¬P(X)∨Q(X,Y)))",
        show (aFNN  form138) ~=? "∃Y.(∃X.(¬P(X)∨¬Q(X,Y)))",
        show (aFNN  form139) ~=? "∃Y.(∃X.(¬P(X)∧¬Q(X,Y)))",
        show (aFNN  form140) ~=? "∃Y.(∃X.(P(X)∧¬Q(X,Y)))",
        show (aFNN  form141) ~=? "∃Y.(∀X.(¬P(X)∨¬Q(X,Y)))"
         ]

-- Tests ejercicio 9
testsFv = test [
         fv form1 ~=? ["x"], --form1  = ¬(¬P(X))
         fv form26 ~=? [],   --form26 = ∀X.(¬(¬P(X)))
         fv form27 ~=? ["y"] --form27 = ∀X.(P(X)∧Q(X,Y))
         ]
-- Tests ejercicio 10
testsEvaluar = test [
        evaluar asignacion1 (fTerm interpretacionNat)  term1 ~=? 2,
        evaluar asignacion2 (fTerm interpretacionNat)  term1 ~=? 21,
        evaluar asignacion1 (fTerm interpretacionZ)  term2 ~=? -4
        ]

-- Tests ejercicio 11
testsActualizarAsignacion = test [
        (actualizarAsignacion "X" 99 asignacion1) "X" ~=? 99,
        (actualizarAsignacion "X" 99 asignacion1) "Y" ~=? 1
        ]

-- Tests ejercicio 12
testVale = test [
        vale interpretacionNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "Y", Var "X"]) ~=? True,
        vale interpretacionNat [0,1] (\x -> if x == "X" then 0 else 1) (Pred "mayor" [Var "X",Func "suc" [Var "X"]]) ~=? False,
        vale interpretacionNat [0,1] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"])) ~=? True,
        vale interpretacionNat [0] (\x -> 0) (E "Y" (Pred "mayor" [Var "Y", Var "X"])) ~=? False,
        vale interpretacionZ [-1,0,1] (\x -> 0) (Y (E "Y" (Pred "mayor" [Var "Y", Var "X"])) (E "Z" (Pred "menor" [Var "Z", Var "X"]))) ~=? True
        ] 
