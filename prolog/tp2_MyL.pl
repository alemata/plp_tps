%Autómatas de ejemplo. Si agregan otros,  mejor.

ejemplo(0, a(s1, [sf], [(s1, a, s1), (s1, b, s1)])).
ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1), (s3, b, s2), (s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11,s28], 
        [(s2, a, s3), (s4, a, s5), (s9, a, s10), (s5, d, s6), (s7, g, s8), (s15, g, s11), (s6, i, s7), (s13, l, s14), (s8, m, s9), (s12, o, s13), (s14, o, s15), (s1, p, s2), (s3, r, s4), (s2, r, s12), (s10, s, s11)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2), (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.

%%Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

%Auxiliar dada en clase
%desde(+X, -Y).
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.


%%Predicados pedidos.

% 1) %esDeterministico(+Automata)

%%-------------------
%%-------------------
divides(X, Y) :- Y mod X =:= 0.

hola(L1, L2) :- include(divides(2), L1, L2).
%%-------------------
%%-------------------

%tuple(X,Y).

listsToTupleList([],[], []).
listsToTupleList([X|Xs],[Y|Ys], L) :- listsToTupleList(Xs, Ys, XYs), append([(X,Y)], XYs, L).

sacarDup([],[]).
sacarDup([X|L],L2) :- member(X,L), sacarDup(L,L2).
sacarDup([X|L],[X|L2]) :- not(member(X,L)), sacarDup(L,L2).

sameList([],[]).
sameList([X|Xs],[Y|Ys]) :- X == Y, sameList(Xs,Ys).

origenDe((O, _, _), O).
etiquetaDe((_, Tag, _), Tag).
destinoDe((_, _, D), D).

origenesAutomata([], []).
origenesAutomata([T|Ts], [O|Os]) :- origenDe(T,O), origenesAutomata(Ts, Os).

etiquetasAutomata([], []).
etiquetasAutomata([T|Ts], [Tag|Tags]) :- etiquetaDe(T,Tag), etiquetasAutomata(Ts, Tags).

destinosAutomata([], []).
destinosAutomata([T|Ts], [D|Ds]) :- destinoDe(T,D), destinosAutomata(Ts, Ds).

origenesYEtiquetasDe(A, L) :- transicionesDe(A, Ts), 
									origenesAutomata(Ts, Os), etiquetasAutomata(Ts, Tags),
									listsToTupleList(Os, Tags, L).

origenesYDestinosDe(A, L) :- transicionesDe(A, Ts), 
									origenesAutomata(Ts, Os), destinosAutomata(Ts, Ds),
									listsToTupleList(Os, Ds, L).

%%-------------------
% 2) esDeterministico(+Automata)
noHayMasDeUnaTransicion(L) :- sacarDup(L, LsinDup), sameList(LsinDup, L).

esDeterministico(A) :- origenesYEtiquetasDe(A, Ts), noHayMasDeUnaTransicion(Ts).

% 2) estados(+Automata, ?Estados)
%% PREGUNTAR ORDEN
estados(A, Estados) :- var(Estados), inicialDe(A, Si), 
				transicionesDe(A, Ts), origenesAutomata(Ts, Os), destinosAutomata(Ts,Ds),
				finalesDe(A, Fs), append([Si|Os], Fs, Es1), append(Es1, Ds, Es), sort(Es, Estados).

estados(A, Estados) :- nonvar(Estados), inicialDe(A, Si), 
				transicionesDe(A, Ts), origenesAutomata(Ts, Os), destinosAutomata(Ts,Ds),
				finalesDe(A, Fs), append([Si|Os], Fs, Es1), append(Es1, Ds, Es),
				forall(member(E, Es), member(E, Estados)).
				


% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)

esCamino(A, Si, Sf, [S1]) :- Si = S1, Sf = S1.

esCamino(A, Si, Sf, [S1|C]) :- Si = S1, last(C,Sf2), Sf = Sf2,
									origenesYDestinosDe(A, Ts), estanEn([S1|C], Ts), !.

estanEn([X,Y], L2) :- member((X,Y), L2), !.
estanEn([X,Y|L1], L2) :- member((X,Y), L2), estanEn([Y|L1], L2). 									

% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% Responder aquí.

% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
%% USAMOS GENERATE & TEST.
caminoDeLongitud(A, N, C, Tags, Si, Sf) :- estados(A, Es), member(Si, Es), member(Sf, Es), transicionesDe(A,T), 
											%crearListaDeEstados(Es, N, C), esCamino(A, Si, Sf, C).
											%crearListaDeEstados(T, N, C), esCamino(A, Si, Sf, C).
											Nm1 is N-1, crearCamino(Si, T, C, Nm1), etiquetasCamino(T,C,Tags).

%crearListaDeEstados(L, 0, []) :- !.
%crearListaDeEstados(L, N, [X|L2]) :- N2 is N - 1, member(X,L), crearListaDeEstados(L,N2,L2).

crearCamino(X, T, [X],0) :- !.
crearCamino(X, T, [X|C], N) :- Nm1 is N-1, member((X,_,Y),T), crearCamino(Y, T, C, Nm1). 

crearListaDeEstados(Ts, 0, []) :- !.
crearListaDeEstados(Ts, 1, [X]) :- member((X,_,_), Ts), !.
crearListaDeEstados(Ts, N, [X,Y|L2]) :- N2 is N - 1, member((X,_,Y), Ts), crearListaDeEstados(Ts,N2,[Y|L2]).

etiquetasCamino(T, [X], []).
etiquetasCamino(T, [X,Y|C], [E|Tags]) :- member((X,E,Y),T), etiquetasCamino(T, [Y|C], Tags).


% 6) alcanzable(+Automata, +Estado)
alcanzable(A, E) :- inicialDe(A, Si), 
					estados(A,Estados),	length(Estados, N), between(2, N, X), !,
					caminoDeLongitud(A, X, _, _, Si, E).

% 7) automataValido(+Automata)
automataValido(_).

%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% 8) hayCiclo(+Automata)
hayCiclo(_).

% 9) reconoce(+Automata, ?Palabra)
reconoce(_, _).

% 10) PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(_, _).

%-----------------
%----- Tests -----
%-----------------

% Algunos tests de ejemplo. Deben agregar los suyos.

test(1) :- forall(ejemplo(_, A),  automataValido(A)).
test(2) :- not((ejemploMalo(_, A),  automataValido(A))).
test(3) :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4) :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5) :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6) :- ejemplo(7, A), not(reconoce(A, [b])).
test(7) :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8) :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a], [b]]).
test(9) :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).
tests :- forall(between(1, 15, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.