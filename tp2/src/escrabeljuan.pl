% Scrabble solitario.
% Diferencias con respecto al juego Scrabble original: 
% 1. Se juega de a uno, buscando obtener el mayor puntaje posible.
% 2. Se juega con todas las fichas que quedan, en lugar de ir sacando de a 7.
% 3. El mismo premio puede utilizarse dos veces, si el casillero se utiliza
%    para dos palabras.
% 4. No hace falta validar que las palabras pertenezcan a un diccionario,
%    aunque un buen jugador utilizará palabras reales.
% 5. Dos palabras pueden tocarse, solaparse o incluso ocupar el mismo espacio
%     - esto último si solo difieren en letras que fueron reemplazadas por *.

% Dado que el juego trae las fichas ch, ll y rr, se tratará a cada una de ellas
% como una letra, siendo estas distintas de [c,h], [l,l] y [r,r].


%%%%%%%%%% Predicados adicionales %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subtract_once(L, [], L).
subtract_once(L1, [H2 | L2], L) :-
    delete_one(H2, L1, Rest),
    subtract_once(Rest, L2, L).

delete_one(_, [], []).
delete_one(X, [X | L], L).
delete_one(X, [H1 | L1], [H1 | L]) :- delete_one(X, L1, L).

equals(L1, L2) :- msort(L1, L1ord), msort(L2, L2ord), L1ord = L2ord.

%%%%%%%%%% Definiciones propias del juego %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Letras y sus puntajes.
puntaje(*, 0).
puntaje(L, 1) :- member(L, [a, e, i, o, u, s, n, l, r, t]).
puntaje(L, 2) :- member(L, [d, g]).
puntaje(L, 3) :- member(L, [b, c, m, p]).
puntaje(L, 4) :- member(L, [h, f, v, y]).
puntaje(L, 5) :- member(L, [ch, q]).
puntaje(L, 8) :- member(L, [j,ll, ñ, rr, x]).
puntaje(L, 10) :- member(L, [z]).

letra(L) :- puntaje(L,_).


% replicar(+N, ?X, ?XS)

% Tiene éxito si XS es una lista con X repetido N veces. Auxiliar para definir
% la lista de fichas brevemente. Eventualmente puede tener otros usos.
replicar(0, _, []).
replicar(N, L, [L|LS]) :- N > 0, Nm1 is N - 1, replicar(Nm1, L, LS).


%     2 fichas en blanco (0 puntos)
%     1 punto: A ×12, E ×12, O ×9, I ×6, S ×6, N ×5, L ×4, R ×5, U ×5, T ×4
%     2 puntos: D ×5, G ×2

%     3 puntos: C ×4, B ×2, M ×2, P ×2
%     4 puntos: H ×2, F ×1, V ×1, Y ×1
%     5 puntos: CH ×1, Q ×1
%     8 puntos: J ×1, LL ×1, Ñ ×1, RR ×1, X ×1
%     10 puntos: Z ×1,
fichas(FS) :-
    replicar(12,a,A), replicar(2,b,B),    replicar(4,c,C),   replicar(5,d,D),
    replicar(12,e,E), replicar(1,f,F),    replicar(2,g,G),   replicar(2,h,H),
    replicar(6,i,I),  replicar(1,j,J),    replicar(4,l,L),   replicar(2,m,M),
    replicar(5,n,N),  replicar(1, ñ, N1), replicar(9,o,O),   replicar(2,p,P),
    replicar(1,q,Q),  replicar(5,r,R),    replicar(6,s,S),   replicar(4,t,T),
    replicar(5,u,U),  replicar(1,v,V),    replicar(1,x,X),   replicar(1,y,Y),
    replicar(1,z,Z),  replicar(1,ch,CH),  replicar(1,ll,LL), replicar(1,rr,RR),
    replicar(2,*,Blancos),
    flatten([A,B,C,D,E,F,G,H,I,J,L,M,N,N1,O,P,Q,R,S,T,U,V,X,Y,Z,CH,LL,RR,Blancos],FS).

%%%%%%%%%%%%%%%%%%%% Definición de matrices %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% matriz(+CantFilas, +CantColumnas, ?Matriz).

% Define Matriz como lista de filas. Se asume que (0,0) es la posición de
% arriba a la izquierda.
matriz(F, _, []) :- F =< 0, !.
matriz(F, C, [L | LS]) :- length(L, C),  matriz(F-1, C, LS). %ver var o letras?

% Pueden usar esto, o comentarlo si viene incluido en su versión de SWI-Prolog.
all_different(L) :- list_to_set(L,L).

%%%%%%%%%% Predicados sobre posiciones en una matriz %%%%%%%%%%%%%%%%%%%%%%%%%%

% enRango(+Tablero, +Posicion)

% La posición indicada está dentro del rango de la matriz del tablero
enRango([F | FS], (X,Y)) :-
    0 =< X,
    0 =< Y,
    length(F, L1), X < L1,
    length(FS, L2), Y =< L2.


% siguiente(?Direccion, +Origen, ?Destino)

% Saber si una posici ́n est ́ definida dentro del tablero, y determinar la
% siguiente posición, en la dirección dada (vertical u horizontal)
siguiente(vertical, (A,B), P) :- I is B+1, P = (A,I).
siguiente(horizontal, (A,B), P) :- I is A+1, P = (I,B).


%%%%%%%%%% Proyectores del tablero %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% matrizDe(+Tablero,?Matriz)
matrizDe(t(M,_,_,_,_,_), M).

% inicialDe(+Tablero,?Inicial)
inicialDe(t(_,I,_,_,_,_), I).

%%%%%%%%%% Predicados para contar fichas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fichasUtilizadas(+Matriz,-Fichas)

% Es importante contar sólo las celdas que no sean variables.
fichasUtilizadas([], []).
fichasUtilizadas([L|Ls], F) :-
     sonFichas(L, X1),
     fichasUtilizadas(Ls, X2),
     append(X1, X2, F).

% sonFichas(+L, ?L1)
sonFichas([], []).
sonFichas([X|Xs], [X|L1]) :- ground(X), sonFichas(Xs, L1), !.
sonFichas([X|Xs], L1) :- not(ground(X)), sonFichas(Xs, L1).


% fichasQueQuedan(+Matriz, -Fichas)
fichasQueQuedan(M, F) :-
    fichas(L1),
    fichasUtilizadas(M, L2),
    subtract_once(L1, L2, F), !.

%%%%%%%%%% Predicados para buscar una letra (con sutiles diferencias) %%%%%%%%%

% letraEnPosicion(+Matriz,?Posicion,?Letra) 

% Letra es lo que hay en Posicion (X,Y), ya sea variable, * o una letra
% propiamente dicha.
letraEnPosicion(M, (X,Y), L) :- nth0(Y, M, F), nth0(X, F, L).


% buscarLetra(+Letra,+Matriz,?Posicion)

% Sólo tiene éxito si en Posicion ya está la letra o un *. No unifica con
% variables.
buscarLetra(X, M, P) :- letraEnPosicion(M, P, X1), ground(X1), X1 = X.


% ubicarLetra(+Letra,+Matriz,?Posicion,+FichasDisponibles,-FichasRestantes) 

ubicarLetra(X, M, P, LD, FR) :-
    delete_one(X, LD, FR), !, 
    letraEnPosicion(M, P, X).
 


% La matriz puede estar parcialmente instanciada.
% El * puede reemplazar a cualquier letra. Puede ubicarla donde había una
% variable.
% Usarlo solo fichas disponibles para que no sea horriblemente ineficiente.
% Las posiciones donde ya estaba la letra son soluciones válidas y no gastan
% una ficha.

% Ejemplo: tablero2(T), matrizDe(T,M), ubicarPalabra([s,i], M, I, horizontal) 
% -> se puede ubicar 'si' horizontalmente de 4 formas distintas
% M = [[s, i, -], [-, -, -], [-, -, -]] ;
% M = [[s, *, -], [-, -, -], [-, -, -]] ;
% M = [[*, i, -], [-, -, -], [-, -, -]] ;
% M = [[*, *, -], [-, -, -], [-, -, -]] ; 
% donde los '-' representan variables, e I es siempre (0,0), ya que es la
% primera palabra de este tablero.

%%%%%%%%%% Predicados para buscar una palabra (con sutiles diferencias) %%%%%%%


% ubicarPalabraConFichas(+Palabra,+Matriz,?Inicial,?Direccion,+FichasDisponibles)

% Auxiliar (opcional), a definir para ubicarPalabra
% La matriz puede estar parcialmente instanciada.
ubicarPalabraConFichas([], _, _, _, _).
ubicarPalabraConFichas([H|T], M, I, D, FD) :-
    member(H, FD),
    ubicarLetra(H, M, I, FD, FR),
    siguiente(D, I, S),
    ubicarPalabraConFichas(T, M, S, D, FR). 


% ubicarPalabra(+Palabra,+Matriz,?Inicial,?Direccion)

% La matriz puede estar parcialmente instanciada.
ubicarPalabra(P, M, I, D) :-
    fichasQueQuedan(M, FD),
    ubicarPalabraConFichas(P, M, I, D, FD). 


% buscarPalabra(+Palabra,+Matriz,?Celdas, ?Direccion)

% Sólo tiene éxito si la palabra ya estaba en la matriz.
buscarPalabra([], _, [], _).
buscarPalabra([X|XS], M, [C|CS], D) :-
    buscarLetra(X, M, C),
    buscarPalabra(XS, M, CS, D),
    direccion_ok([C|CS], D).

% direccion(+Posiciones, ?Direccion)
direccion_ok([_|[]], vertical).
direccion_ok([_|[]], horizontal).
direccion_ok([C1,C2|CS], D) :-
    siguiente(D, C1, C2),
    direccion_ok([C2|CS], D), !.
 

% celdasPalabra(+Palabra,+Matriz,-Celdas)

% Similar a buscarPalabra, pero también permite ubicar letras en espacios
% libres. Opcional ya definida.
celdasPalabra(Palabra, M, [C|CS]) :-
    ubicarPalabra(Palabra, M, C, D),
    buscarPalabra(Palabra, M, [C|CS], D).


%%%%%%%%%%Predicados para validar el tablero y los juegos %%%%%%%%%%%%%%%%%%%%%

% tableroValido(+Matriz, +Inicial, +ListaDL, +ListaDP, +ListaTL, +ListaTP)
tableroValido(M, (C,F), LDL, LDP, LTL, LTP) :-
    dimensiones(M, CantFil, CantCol), 

    C < CantCol, F < CantFil,         % Casilla inicial esta dentro del tablero

    flatten([LDL, LDP, LTL, LTP], L), % Casilleros premiados estan dentro del
    premios_ok(L, CantFil, CantCol).  % tablero y no se repiten.   
    
dimensiones([], 0, 0).
dimensiones([X|XS], CantFil, CantCol) :-
    length(X, CantCol),
    length([X|XS], CantFil).

premios_ok([], _, _).
premios_ok([(C,F)|XS], CantFil, CantCol) :-
    not(member((C,F), XS)), C < CantCol, F < CantFil,  
    premios_ok(XS, CantFil, CantCol).


% seCruzan(+Palabra1,+Palabra2,+Matriz)
seCruzan(Palabra1, Palabra2, M) :-
    buscarPalabra(Palabra2, M, CS2,D2),
    buscarPalabra(Palabra1, M, CS1,D1),
    D1 \= D2,
    member(C, CS1), member(C, CS2), !.

% cruzaAlguna(+Palabra,+Anteriores,+Matriz)
cruzaAlguna(Palabra, Anteriores, M) :-
    member(P, Anteriores),
    seCruzan(Palabra, P, M).

% juegoValido(+?Tablero, +Palabras)
juegoValido(t(M,I,LDL,LDP,LTL,LTP), P) :-
    tableroValido(M,I,LDL,LDP,LTL,LTP),
    juegoValidoConPalabras(T, [], P).


% juegoValidoConPalabras(+Tablero, +PalabrasAUsar, +PalabrasUsadas)
juegoValidoConPalabras(_, [], _), !.
juegoValidoConPalabras(_, [XS|[]], XS), !.
juegoValidoConPalabras(T, [XS|XSS], [XS|PUS]) :-
    not(cruzaAlgula(XS, PUS)),
    juegoValidoConPalabras(T, XSS, [XS|PUS]).
    


%%%%%%%%%% Predicados para calcular puntajes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% puntajePalabra(+Palabra, +Tablero, -Puntos)
puntajePalabra([], _, 0).
%% puntajePalabra(Palabra, t(M,I,LDL,LDP,LTL,LTP), Puntos) :-
%%     buscarPalabra(Palabra, M, Posiciones, _),
%%     bonusPalabra(Posiciones, LDP, LTP, BonusPalabra),
%%     puntos(Palabra, Posiciones, LDL, LTL, PuntosTmp),
%%     Puntos is PuntosTmp * BonusPalabra.

%% % puntos(Palabra, Posiciones, ListaDL, ListaDP, Puntos)
%% puntos([X|XS],[Y|YS], LDL, LTL, Puntos) :-
%%     puntaje(X, Tmp),
%%     member(X, LDL), 
%%     puntos(XS, LDL, LTL, PuntosTmp),
%%     PuntosTmp2 is Tmp * PremiosPalabra + PuntosTmp,
    


% puntajeJuego(+Tablero, +Palabras, -Puntaje)


%%%%%%%%%% Predicados para copiar estructuras (HECHOS) %%%%%%%%%%%%%%%%%%%%%%%%

% copiaMatriz(+Matriz,-Copia)

% Copia el contenido de las celdas que no son variables, y a las otras las
% llena con nuevas variables.
copiaMatriz(Matriz,Copia) :- maplist(copiaFila, Matriz, Copia).


% copiaFila(+Fila,-Copia)

%Copia una fila, manteniendo el contenido de las celdas ya instanciadas, y
%generando nuevas variables para las otras.
copiaFila([],[]).
copiaFila([C|CS1],[C|CS2]) :- nonvar(C), copiaFila(CS1,CS2).
copiaFila([C|CS1],[_|CS2]) :- var(C), copiaFila(CS1,CS2).


% copiaTablero(+Tablero,-Copia)
copiaTablero(t(M1, I, DLS, DPS, TLS, TPS),t(M2, I, DLS, DPS, TLS, TPS)) :-
    copiaMatriz(M1,M2).

%%%%%%%%%% Para obtener una solución óptima %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% juegoPosible(+TableroInicial,+Palabras,-TableroCompleto,-Puntaje)


% juegoOptimo(+TableroInicial,+Palabras,-TableroCompleto,-Puntaje)

% La conversa de una solución suele ser solución a menos que los premios
% favorezcan a una de ellas.


%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TEST 01 - Da 1 solución de 66 puntos.
% tablero1(T), juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos).
%% tablero1(T), juegoPosible(T,[[p,a,z],[p,e,z],[z,a,r]],CT,66), matrizDe(CT,M), buscarPalabra([p,a,z],M,C1,_), buscarPalabra([p,e,z],M,C2,_),buscarPalabra([z,a,r],M,C3,_).

% TEST 02 - Da 2 soluciones de 52 puntos:
% tablero1(T), juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos).

% TEST 03 - Da 2 soluciones de 88 puntos:
% tablero4(T),juegoOptimo(T,[[p,a,n],[p,e,z],[a,g,u,a]],Sol,Puntos), matrizDe(Sol,M), buscarPalabra([p,a,n],M,C1,_), buscarPalabra([p,e,z],M,C2,_),buscarPalabra([a,g,u,a],M,C3,_).

% TEST 04 - Da 2 soluciones de 38 puntos:
% tablero2(T), juegoOptimo(T,[[p,a,n],[p,e,z]],CT,Puntos).

% TEST 05 - Da 2 soluciones de 52 puntos, que usan *.
% tablero2(T), juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos).

% TEST 06 - Da 3 soluciones de 91 puntos.
% tablero2(T), juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos).


testJuegoOptimo1 :-
    tablero1(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos),XS),
    XS=[(_,66)].

testJuegoOptimo2 :-
    tablero1(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos),XS),
    length(XS,2),
    XS=[(_,52)|_].

testJuegoOptimo3 :-
    tablero4(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,n],[p,e,z],[a,g,u,a]],CT,Puntos),XS),
    length(XS,2),
    XS=[(_,88)|_].

testJuegoOptimo4 :-
    tablero2(T), findall((CT,Puntos),juegoOptimo(T,[[p,a,n],[p,e,z]],CT,Puntos),XS),
    length(XS,2),
    XS=[(_,38)|_].

testJuegoOptimo5 :-
    tablero2(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z]],CT,Puntos),XS),
    length(XS,2),
    XS=[(_,52)|_].

testJuegoOptimo6 :-
    tablero2(T),
    findall((CT,Puntos),juegoOptimo(T,[[p,a,z],[p,e,z],[z,a,r]],CT,Puntos),XS),
    length(XS,3),
    XS=[(_,91)|_].

testJuegoOptimo :-
    testJuegoOptimo1,
    testJuegoOptimo2,
    testJuegoOptimo3,
    testJuegoOptimo4,
    testJuegoOptimo5,
    testJuegoOptimo6.

