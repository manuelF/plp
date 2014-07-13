
%%%%%%%%%% Tableros %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Tableros facilitados por la catedra

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

% Tableros pequeños para pruebas.
tablero2(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    matriz(3, 3, M).

tablero3(t(M,(0,2),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    matriz(3, 3, M).

tablero4(t(M,(2,2),[(1,1),(1,3),(3,1),(3,3)],[(0,0),(2,2),(4,4)],
	   [(0,2),(2,0),(2,4),(4,2)],[(0,4),(4,0)])) :-
    matriz(5, 5, M).

%%%%% Nuevos tableros para probar puntajePalabra y puntajeJuego

tablero5(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    M = [[c,_,_],[_,_,_],[_,_,_]].

tablero6(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    M = [[_,_,_], [_,g,o], [_,_,_]].

tablero7(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    M = [[_,_,_], [_,_,i], [_,_,z]].

tablero8(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    M = [[_,_,_], [_,j,_], [_,e,_]].

tablero9(t(M,(0,0),[(1,2),(2,1)],[(0,0)],[(1,1)],[(2,2)])) :-
    M = [[p,a,z], [e,_,_], [z,_,_]].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(escrabel).

test(matriz) :-
    matriz(2, 2, [[_,_],[_,_]]),
    matriz(0, 0, []).

test(siguiente) :-
    siguiente(vertical, (0,0), (0,1)),
    siguiente(horizontal, (0,0), (1,0)),
    siguiente(vertical, (0,1), P1), P1 = (0,2),
    siguiente(horizontal, (0,0), P2), P2 = (1,0).

test(fichasUtilizadas) :-
    fichasUtilizadas([[_,_], [_,_]], L1), L1 = [],
    fichasUtilizadas([[_,a], [b,_]], L2), equals(L2, [a,b]),
    fichasUtilizadas([[a,a], [b,_]], L3), equals(L3, [a,b,a]).

test(fichasQueQuedan) :-
    fichasQueQuedan([[_,_], [_,_]], L1), fichas(F), equals(L1, F),
    fichasQueQuedan([[a,b], [a,c]], L2), subtract_once(F, [a,b,a,c], L3), equals(L2, L3).

test(buscarLetra) :-
    buscarLetra(a, [[X1,a], [b,X2]], (1,0)),
    buscarLetra(a, [[X1,a], [b,X2]], P1), P1 = (1,0),
    buscarLetra(a, [[*,_], [_,_]], P2), P2 = (0,0).

test(ubicarLetra) :-
    ubicarLetra(a, [[b,b],[b,_]], Pos, [a,b,c], [b,c]), Pos == (1,1).

test(ubicarPalabra) :-
    tablero2(t(M2,I2,_,_,_,_)), ubicarPalabra([p,e,z], M2, I2, _).

test(buscarPalabra) :-
    buscarPalabra([p,e], [[p,_],[e,_]], [(0,0),(0,1)], vertical),
    buscarPalabra([p,e], [[p,e],[_,_]], [(0,0),(1,0)], horizontal).

test(tableroValido) :-
    tablero1(t(M1,I1,LDL1,LDP1,LTL1,LTP1)), tableroValido(M1,I1,LDL1,LDP1,LTL1,LTP1),
    tablero2(t(M2,I2,LDL2,LDP2,LTL2,LTP2)), tableroValido(M2,I2,LDL2,LDP2,LTL2,LTP2),
    tablero3(t(M3,I3,LDL3,LDP3,LTL3,LTP3)), tableroValido(M3,I3,LDL3,LDP3,LTL3,LTP3),
    tablero3(t(M3,I3,LDL3,LDP3,LTL3,LTP3)), tableroValido(M3,I3,LDL3,LDP3,LTL3,LTP3),
    tablero4(t(M4,I4,LDL4,LDP4,LTL4,LTP4)), tableroValido(M4,I4,LDL4,LDP4,LTL4,LTP4).

test(juegoValido) :-
    tablero2(T0), juegoValido(T0, [[d,a,o],[p,e],[p,e,d]]),  
    tablero2(T1), juegoValido(T1, [[p,e,z],[p,a,n]]),
    tablero2(T2), juegoValido(T2, [[p,a,c],[p,e,c],[a,t,e]]),
    tablero3(T3), juegoValido(T3, [[p,a,z],[p,e,s],[s,a,r]]).
 
test(puntajePalabra) :-
    tablero5(T5), puntajePalabra([c], T5, Puntos5), Puntos5 is 6,
    tablero6(T6), puntajePalabra([g,o], T6, Puntos6), Puntos6 is 8,
    tablero7(T7), puntajePalabra([i,z], T7, Puntos7), Puntos7 is 36,
    tablero8(T8), puntajePalabra([j,e], T8, Puntos8), Puntos8 is 26,
    tablero9(T9_1), puntajePalabra([p,a,z], T9_1, Puntos9_1), Puntos9_1 is 28,
    tablero9(T9_2), puntajePalabra([p,e,z], T9_2, Puntos9_2), Puntos9_2 is 28.

test(puntajeJuego) :-
    tablero5(T5), puntajeJuego(T5, [[c]], Puntos5), Puntos5 is 6,
    tablero6(T6), puntajeJuego(T6, [[g,o]], Puntos6), Puntos6 is 8,
    tablero7(T7), puntajeJuego(T7,[[i,z]], Puntos7), Puntos7 is 36,
    tablero2(T2_1), puntajeJuego(T2_1, [[p,e,z],[p,a,z]], Puntos2_1), Puntos2_1 is 56 .


%% %test(juegoPosible) :-

:- end_tests(escrabel).


testJuegoOptimo1 :-
    tablero1(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos),XS),
    length(XS,8), XS=[(_,92)|_].

testJuegoOptimo2 :-
    tablero1(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos),XS),
    length(XS,2), XS=[(_,60)|_].

testJuegoOptimo3 :-
    tablero4(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,n],[p,e,z],[a,g,u,a]],CT,Puntos),XS),
    length(XS,2),XS=[(_,88)|_].

testJuegoOptimo4 :-
    tablero2(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,n],[p,e,z]],CT,Puntos),XS),
    length(XS,2),XS=[(_,44)|_].

testJuegoOptimo5 :-
    tablero2(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos),XS),
    length(XS,2),XS=[(_,60)|_].

testJuegoOptimo6 :-
    tablero2(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos),XS),
    length(XS,12),XS=[(_,91)|_].

testJuegoOptimo :-
    testJuegoOptimo1,
    testJuegoOptimo2,
    testJuegoOptimo3,
    testJuegoOptimo4,
    testJuegoOptimo5,
    testJuegoOptimo6.


% TEST 01 - Da 8 soluciones de 92 puntos.
%tablero1(T), juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos).
%tablero1(T), juegoPosible(T,[[p,a,z],[p,e,z],[z,a,r]],CT,66), matrizDe(CT,M), buscarPalabra([p,a,z],M,C1,_), buscarPalabra([p,e,z],M,C2,_),buscarPalabra([z,a,r],M,C3,_).

% TEST 02 - Da 2 soluciones de 60 puntos:
% tablero1(T), juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos).

% TEST 03 - Da 2 soluciones de 88 puntos
% tablero4(T),juegoOptimo(T,[[p,a,n],[p,e,z],[a,g,u,a]],Sol,Puntos), matrizDe(Sol,M), buscarPalabra([p,a,n],M,C1,_), buscarPalabra([p,e,z],M,C2,_),buscarPalabra([a,g,u,a],M,C3,_).

% TEST 04 - Da 2 soluciones de 44 puntos:
% tablero2(T), juegoOptimo(T,[[p,a,n],[p,e,z]],CT,Puntos).

% TEST 05 - Da 2 soluciones de 60 puntos, que usan *.
% tablero2(T), juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos).

% TEST 06 - Da 12 soluciones de 91 puntos.
% tablero2(T), juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos).
