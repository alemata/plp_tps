%Autómatas de ejemplo. Si agregan otros, mejor.

ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1), (s3, b, s2), (s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11],
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
esDeterministico(Automata):-
  transicionesDe(Automata,T),
  member((O1,E1,D1), T),
  member((O2,E2,D2),T),
  O1 = O2,
  D1 \= D2,
  E1 = E2,
  !,
  fail.
esDeterministico(Automata).


% 2) estados(+Automata, ?Estados)
estados(Automata, Estados):-
  nonvar(Estados),
  todosLosNodos(Automata, Nodos),
  member(X, Nodos),
  \+member(X, Estados),
  !,
  fail.
estados(Automata, Estados):-
  nonvar(Estados),!.
estados(Automata, Estados):-
  var(Estados),
  setof((X), (todosLosNodos(Automata, LF), member(X, LF)), Estados).

% Nodos = lista de nodos del automata sin repetidos.
% todosLosNodos(+Automata, -Nodos)
todosLosNodos(Automata, Nodos):-
  inicialDe(Automata, I),
  finalesDe(Automata, F),
  transicionesDe(Automata,T),
  nodos(T, N),
  append([I|F], N, NodosConRepetidos),
  setof(X, (member(X, NodosConRepetidos)), Nodos).

% ahhhhhhhh porque no anda? nodos(T, N):-setof((_,_X), (member((_,_,X), T)), N).
% Nodos = nodos de las transiciones.
% nodos(+Transiciones, -Nodos)
nodos([], []).
nodos([(X,Y,W)|T], [X,W|Nodos]):-

  nodos(T, Nodos).


esCamino(Automata, EstadoInicial, EstadoFinal, [X]):-
  EstadoInicial = X,
  EstadoFinal = X, !.
  % feo para mi (mismo pattern matching)... mejor poner un if...
esCamino(Automata, EstadoInicial, EstadoFinal, [X,Y|Resto]):-
  EstadoInicial = X,
  transicionesDe(Automata, T),
  member((X,_,Y), T),
  !, % si hay varias etiquetas no importa para saber si es camino.
  esCamino(Automata, Y, EstadoFinal, [Y|Resto]).

% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)
%esCamino(Automata, EstadoInicial, EstadoFinal, [X]):-
%  EstadoInicial = X,
%  EstadoFinal = X,
%  !.
%esCamino(Automata, EstadoInicial, EstadoFinal, [X,Y|Resto]):-
%  EstadoInicial = X,
%  transicionesDe(Automata, T),
%  \+member((X,_,Y), T),
%  !,
%  fail.
%  % feo para mi (mismo pattern matching)... mejor poner un if...
%esCamino(Automata, EstadoInicial, EstadoFinal, [X,Y|Resto]):-
%  EstadoInicial = X,
%  esCamino(Automata, Y, EstadoFinal, [Y|Resto]).


% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% Responder aquí.

% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
caminoDeLongitud(Automata, N, Camino, Etiquetas, S1, S2):-
  estados(Automata, FL),
  member(S1, FL),
  member(S2, FL),
  posiblesCaminos(Automata, N, Camino),
  esCamino(Automata, S1, S2, Camino),
  etiquetasCamino(Automata, Camino, Etiquetas).

posiblesCaminos(Automata,0, []).
posiblesCaminos(Automata,N, [X,Y|C2]):-
  N2 is N - 2,
  length(C2, N2),
  estados(Automata, FL),
  member(X,FL),
  transicionesDe(Automata, T),
  member((X,_,Y), T),
  posiblesCaminos(Automata,N2,C2).

etiquetasCamino(Automata, [X], []).
etiquetasCamino(Automata, [X,Y|Rest], [E|RE]):-
  length([X,Y|Rest], N),
  transicionesDe(Automata, T),
  member((X,E,Y), T ),
  etiquetasCamino(Automata, [Y|Rest], RE).
  

% 6) alcanzable(+Automata, +Estado)
alcanzable(Automata, Estado):-
  inicialDe(Automata, EstadoInicial),
  estados(Automata, Estados),
  length(Estados, L),
  between(2, L, X),
  caminoDeLongitud(Automata, X, Camino, Etiquetas, EstadoInicial, Estado),
  !.

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