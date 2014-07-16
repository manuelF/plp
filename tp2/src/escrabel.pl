%/* vim: set filetype=prolog : */

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

% Hace exacatamente lo mismo que length pero con los argumentos dados vuelta
% Util para poder usar en maplist.
% length(?Int, ?List)
length_(N, L) :- length(L, N).


% Verdadero cuando  Lista es igual a Lista1 menos Lista2.
% subtract_once(+Lista1, +Lista2, Lista)
subtract_once(L, [], L).
subtract_once(L1, [H2 | L2], L) :-
    %delete_one(H2, L1, Rest),
    selectchk(H2, L1, Rest),
    subtract_once(Rest, L2, L).


equals(L1, L2) :- msort(L1, L1ord), msort(L2, L2ord), L1ord = L2ord.


% Pueden usar esto, o comentarlo si viene incluido en su versión de SWI-Prolog.
all_different(L) :- list_to_set(L,L).

%%%%%%%%%%%%%%%%%%%%%% Definiciones propias del juego %%%%%%%%%%%%%%%%%%%%%%%%%%

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

%Tiene éxito si XS es una lista con X repetido N veces. Auxiliar para definir
%la lista de fichas brevemente. Eventualmente puede tener otros usos.
% replicar(+N, ?X, ?XS)
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

%%%%%%%%%%% Definición de matrices %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define Matriz como lista de filas. Se asume que (0,0) es la posición de arriba
% a la izquierda.
% matriz(+Filas, +Columnas, ?Matriz).
matriz(F, C, M) :- length(M, F), maplist(length_(C), M).


%%%%%%%%%% Predicados sobre posiciones en una matriz %%%%%%%%%%%%%%%%%%%%%%%%%%

% La posición indicada está dentro del rango de la matriz del tablero
% enRango(+Tablero, +Posicion)
enRango([F | FS], (X,Y)) :-
    0 =< X,
    0 =< Y,
    length(F, L1), X < L1,
    length(FS, L2), Y =< L2.


% Saber si una posicion esta definida dentro del tablero, y determinar la
% siguiente posicion, en la dirección dada (vertical u horizontal)
% siguiente(?Direccion, +Origen, ?Destino)
siguiente(vertical, (A,B), P) :- I is B+1, P = (A,I).
siguiente(horizontal, (A,B), P) :- I is A+1, P = (I,B).


%%%%%%%%%% Proyectores del tablero %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% matrizDe(+Tablero,?Matriz)
matrizDe(t(M,_,_,_,_,_), M).

% inicialDe(+Tablero,?Inicial)
inicialDe(t(_,I,_,_,_,_), I).

%%%%%%%%%% Predicados para contar fichas %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Es importante contar sólo las celdas que no sean variables.
% fichasUtilizadas(+Matriz,-Fichas)
fichasUtilizadas([], []).
fichasUtilizadas([L|Ls], F) :-
     sonFichas(L, X1),
     fichasUtilizadas(Ls, X2),
     append(X1, X2, F).

% sonFichas(+L, ?L1)
sonFichas([], []).
sonFichas([X|Xs], [X|L1]) :- nonvar(X), sonFichas(Xs, L1).
sonFichas([X|Xs], L1) :- var(X), sonFichas(Xs, L1).

% fichasQueQuedan(+Matriz, -Fichas)
fichasQueQuedan(M, F) :-
    fichas(L1),
    fichasUtilizadas(M, L2),
    subtract_once(L1, L2, F).

%%%%%%%%%% Predicados para buscar una letra (con sutiles diferencias) %%%%%%%%%

%  Letra es lo que hay en Posicion (X,Y), ya sea variable, * o una letra
%  propiamente dicha.
% letraEnPosicion(+Matriz,?Posicion,?Letra)
letraEnPosicion(M,(X,Y),L) :- nth0(Y,M,F), nth0(X,F,L).


% Sólo tiene éxito si en Posicion ya está la letra o un *. No unifica con
% variables.
% buscarLetra(+Letra,+Matriz,?Posicion)
buscarLetra(X, M, P) :- letraEnPosicion(M, P, X1), nonvar(X1), X1 = X.
buscarLetra(X, M, P) :- X \= '*', letraEnPosicion(M, P, X1), nonvar(X1), X1 = '*'.


% ubicarLetra(+Letra,+Matriz,?Posicion,+FichasDisponibles,-FichasRestantes)
ubicarLetra(X, M, P, FD, FD) :-
    buscarLetra(X, M, P).
ubicarLetra(X, M, P, FD, FR) :-
    letraEnPosicion(M, P, X1), var(X1),
    X1 = X,
    selectchk(X, FD, FR).
ubicarLetra(_, M, P, FD, FR) :-
    letraEnPosicion(M, P, X1), var(X1),
    X1 = '*',
    selectchk('*', FD, FR).

%%%%%%%%%% Predicados para buscar una palabra (con sutiles diferencias) %%%%%%%


% ubicarPalabraConFichas(+Palabra,+Matriz,?Inicial,?Direccion,+FichasDisponibles)
ubicarPalabraConFichas([], _, _, _, _).
ubicarPalabraConFichas([H|T], M, I, D, FD) :-
    ubicarLetra(H, M, I, FD, FR),
    enRango(M, I),
    siguiente(D, I, S),
    ubicarPalabraConFichas(T, M, S, D, FR).


% ubicarPalabra(+Palabra,+Matriz,?Inicial,?Direccion)
ubicarPalabra(P, M, I, D) :-
    fichasQueQuedan(M, FD),
    ubicarPalabraConFichas(P, M, I, D, FD).


% Sólo tiene éxito si la palabra ya estaba en la matriz.
% buscarPalabra(+Palabra,+Matriz,?Celdas, ?Direccion)
buscarPalabra([], _, [], _).
buscarPalabra([X|[]], M, [C|[]], _) :- buscarLetra(X, M, C).
buscarPalabra([X1,X2|XS], M, [C1,C2|CS], D) :-
    buscarLetra(X1, M, C1),
    siguiente(D, C1, C2), 
    buscarPalabra([X2|XS], M, [C2|CS], D).


% Similar a buscarPalabra, pero también permite ubicar letras en espacios
% libres. Opcional ya definida.
% celdasPalabra(+Palabra,+Matriz,-Celdas)
celdasPalabra(Palabra, M, [C|CS]) :-
    ubicarPalabra(Palabra, M, C, D),
    buscarPalabra(Palabra, M, [C|CS], D).


%%%%%%%%%%Predicados para validar el tablero y los juegos %%%%%%%%%%%%%%%%%%%%%

% tableroValido(+Matriz, +Inicial, +ListaDL, +ListaDP, +ListaTL, +ListaTP)
tableroValido(M, (C,F), LDL, LDP, LTL, LTP) :-
    enRango(M, (C,F)),
    flatten([LDL, LDP, LTL, LTP], Premios),
    all_different(Premios),
    maplist(enRango(M), Premios).


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


%juegoValido(+?Tablero, +Palabras)
juegoValido(t(M,I,LDL,LDP,LTL,LTP), [X|XS]) :-
       tableroValido(M, I, LDL, LDP, LTL, LTP),
       ubicarPalabra(X, M, I, _),
       juegoValidoConPalabras(t(M,I,LDL,LDP,LTL,LTP), XS, [X]).

juegoValidoConPalabras(_, [], _).
juegoValidoConPalabras(t(M,I,LDL,LDP,LTL,LTP), [X|XS], YS) :-
      ubicarPalabra(X, M, _, _),
      cruzaAlguna(X, YS, M),
      juegoValidoConPalabras(t(M,I,LDL,LDP,LTL,LTP), XS, [X|YS]).


%%%%%%%%%% Predicados para calcular puntajes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consigue las posiciones donde esta la palabra en el tablero, luego
% consigue los bonus por letra y por la palabra en general y calcula
% la puntuacion final.
% puntajePalabra(+Palabra, +Tablero, -Puntos)
puntajePalabra([], _, 0).
puntajePalabra(Palabra, t(M,_,LDL,LDP,LTL,LTP), Puntos) :-
    buscarPalabra(Palabra, M, Posiciones, _),
    bonusPalabra(Posiciones, LDP, LTP, BonusPalabra),
    bonusLetras(Posiciones,  LDL, LTL, BonusLetras),
    puntosPalabra(M, Posiciones, BonusLetras, PuntosTmp),
    Puntos is PuntosTmp * BonusPalabra.

% Suma los valores de cada letra de la palabra multiplicada por los bonuses
% dados en la otra lista.
% puntosPalabra(+Matriz, +Palabra, +LBonus, ?Puntos).
puntosPalabra(_, [], [], 0).
puntosPalabra(M, [X|XS], [Y|YS], Puntos) :-
    letraEnPosicion(M, X, L),
    puntaje(L, PuntosLetra),
    PuntosLetraConBonus is PuntosLetra * Y,
    puntosPalabra(M, XS, YS, PuntosRestantes),
    Puntos is PuntosLetraConBonus + PuntosRestantes.

% bonusPalabra(+Posiciones, +LPosicionesDobles, +LPosicionesTriples, -LBonus)
bonusPalabra([], _, _, 1).
bonusPalabra([X|XS], LDP, LTP, BP) :-
    member(X, LDP),
    bonusPalabra(XS, LDP, LTP, BPTmp),
    BP is BPTmp * 2.
bonusPalabra([X|XS], LDP, LTP, BP) :-
    member(X, LTP),
    bonusPalabra(XS, LDP, LTP, BPTmp),
    BP is BPTmp * 3.
bonusPalabra([X|XS], LDP, LTP, BPTmp) :-
    \+ member(X, LDP),
    \+ member(X, LTP),
    bonusPalabra(XS, LDP, LTP, BPTmp).

% bonusLetras(+LPosiciones, +LPosicionesDobles, +LPosicionesTriples, -LBonus)
bonusLetras([], _, _, []).
bonusLetras([X|XS], LDL, LTL, [2|LBLS]) :-
    member(X, LDL),
    bonusLetras(XS, LDL, LTL, LBLS).
bonusLetras([X|XS], LDL, LTL, [3|LBLS]) :-
    member(X, LTL),
    bonusLetras(XS, LDL, LTL, LBLS).
bonusLetras([X|XS], LDL, LTL, [1|LBLS]) :-
    \+ member(X, LDL),
    \+ member(X, LTL),
    bonusLetras(XS, LDL, LTL, LBLS).


% puntajeJuego(+?Tablero, +Palabras, -Puntaje)
puntajeJuego(Tablero, Palabras, Puntaje) :-
    juegoValido(Tablero, Palabras),
    puntajeTotal(Palabras, Tablero, Puntaje).

% puntajeTotal(+Palabras, +Tablero, -Puntaje)
puntajeTotal([], _, 0).
puntajeTotal([X|XS], Tablero, Puntaje) :-
    puntajePalabra(X, Tablero, PuntajePalabra),
    puntajeTotal(XS, Tablero, PuntajeRestante),
    Puntaje is PuntajePalabra + PuntajeRestante.



%%%%%%%%%% Predicados para copiar estructuras (HECHOS) %%%%%%%%%%%%%%%%%%%%%%%%


% Copia el contenido de las celdas que no son variables, y a las otras las
% llena con nuevas variables.
% copiaMatriz(+Matriz,-Copia)
copiaMatriz(Matriz,Copia) :- maplist(copiaFila, Matriz, Copia).


%Copia una fila, manteniendo el contenido de las celdas ya instanciadas, y
%generando nuevas variables para las otras.
% copiaFila(+Fila,-Copia)
copiaFila([],[]).
copiaFila([C|CS1],[C|CS2]) :- nonvar(C), copiaFila(CS1,CS2).
copiaFila([C|CS1],[_|CS2]) :- var(C), copiaFila(CS1,CS2).


% copiaTablero(+Tablero,-Copia)
copiaTablero(t(M1, I, DLS, DPS, TLS, TPS),t(M2, I, DLS, DPS, TLS, TPS)) :-
    copiaMatriz(M1,M2).

%%%%%%%%%% Para obtener una solución óptima %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% juegoPosible(+TableroInicial,+Palabras,-TableroCompleto,-Puntaje)
juegoPosible(TableroInicial, Palabras, TableroCompleto, Puntaje) :-
    copiaTablero(TableroInicial, TableroCompleto),
    puntajeJuego(TableroCompleto, Palabras, Puntaje).


% La conversa de una solución suele ser solución a menos que los premios
% favorezcan a una de ellas.
% juegoOptimo(+TableroInicial,+Palabras,-TableroCompleto,-Puntaje)
juegoOptimo(TableroInicial, Palabras, MejorTableroCompleto, MejorPuntaje) :-
    findall(P, juegoPosible(TableroInicial, Palabras, _, P), XS),
    max_list(XS, PuntajeMaximo),
    juegoPosible(TableroInicial, Palabras, MejorTableroCompleto, MejorPuntaje),
    MejorPuntaje = PuntajeMaximo.


