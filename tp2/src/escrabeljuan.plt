%%%%%%%%%% Tableros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hagan algunos tableros inválidos para probar tableroValido.

% Tablero tradicional de Scrabble.
tablero1(t(M, (7,7), DLS, DPS, TLS, TPS)) :-
    matriz(15, 15, M),
    TPS=[(0,0), (0,7), (0,14), (7,0), (7,14),
	 (14,0), (14,7), (14,14)],

    DPS=[(1,1),(2,2),(3,3),(4,4),(7,7),(10,10),
	 (11,11),(12,12),(13,13),(13,1),(12,2),
	 (11,3),(10,4),(4,10),(3,11),(2,12),(1,13)],

    TLS=[(1,5),(1,9),(5,1),(5,5),(5,9),(5,13),
	 (9,1),(9,5),(9,9),(9,13),(13,5),(13,9)],

    DL1=[(0,3),(0,11),(3,0),(3,14),(11,0),(11,14),
	 (14,3),(14,11),(6,6),(8,8),(8,6),(6,8)],

    DL2=[(2,6),(2,8),(3,7),(6,2),(8,2),(7,3),(12,6),
	 (12,8),(11,7),(6,12),(8,12),(7,11)],

    append(DL1,DL2,DLS).

%Tableros pequeños para pruebas.
tablero2(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    matriz(3, 3, M).

tablero3(t(M,(0,2),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    matriz(3, 3, M).

tablero4(t(M,(2,2),[(1,1),(1,3),(3,1),(3,3)],[(0,0),(2,2),(4,4)],
	   [(0,2),(2,0),(2,4),(4,2)],[(0,4),(4,0)])) :-
    matriz(5, 5, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(escrabel).

test(siguiente) :-
    siguiente(vertical, (0,0), (0,1)),
    siguiente(horizontal, (0,0), (1,0)),
    siguiente(vertical, (0,1), P1), P1 = (0,2),
    siguiente(horizontal, (0,0), P2), P2 = (1,0).

test(fichasUtilizadas) :-
    fichasUtilizadas([[_,_], [_,_]], L1), L1 = [], 
    fichasUtilizadas([[_,a], [b,_]], L2), equals(L2, [a,b]).

test(fichasQueQuedan) :-
    fichasQueQuedan([[_,_], [_,_]], L), fichas(F), equals(L, F).
	       
test(buscarLetra) :-
    buscarLetra(a, [[X1,a], [b,X2]], (1,0)),
    buscarLetra(a, [[X1,a], [b,X2]], P), P = (1,0).

test(tableroValido) :-
    tablero2(t(M2,I2,LDL2,LDP2,LTL2,LTP2)), tableroValido(M2,I2,LDL2,LDP2,LTL2,LTP2),
    tablero3(t(M3,I3,LDL3,LDP3,LTL3,LTP3)), tableroValido(M3,I3,LDL3,LDP3,LTL3,LTP3),
    tablero3(t(M3,I3,LDL3,LDP3,LTL3,LTP3)), tableroValido(M3,I3,LDL3,LDP3,LTL3,LTP3),
    tablero4(t(M4,I4,LDL4,LDP4,LTL4,LTP4)), tableroValido(M4,I4,LDL4,LDP4,LTL4,LTP4).

%test(puntajePalabra) :-
    

:- end_tests(escrabel).
