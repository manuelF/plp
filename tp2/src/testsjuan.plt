
equals(L1, L2) :- msort(L1, L1ord), msort(L2, L2ord), L1ord = L2ord.

:- begin_tests(escrabel).

test(siguiente) :-
    siguiente(vertical, (0,0), (0,1)),
    siguiente(horizontal, (0,0), (1,0)),
    siguiente(vertical, (0,1), P1), P1 = (0,2),
    siguiente(horizontal, (0,0), P2), P2 = (1,0).

test(fichasUtilizadas) :-
    fichasUtilizadas([[X1,X2], [X3,X4]], L1), L1 = [], 
    fichasUtilizadas([[X1,a], [b,X2]], L2), equals(L2, [a,b]).

test(fichasQueQuedan) :-
    fichasQueQuedan([[X1,X2], [X3,X4]], L), fichas(F), equals(L, F).
	       

:- end_tests(escrabel).
