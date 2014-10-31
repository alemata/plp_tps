%Autómatas de ejemplo. Si agregan otros,  mejor.

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

%Funciones auxiliares

% Se eliminan los elementos duplicados de la primer lista L1.
%sacarDup(+L1, -L2).
sacarDup([],[]).
sacarDup([X|L],L2) :- member(X,L), !, sacarDup(L,L2).
sacarDup([X|L],[X|L2]) :- not(member(X,L)), sacarDup(L,L2).

% Si ambas están definidas, retorna true si las dos son iguales.
% Si una está instanciada y la otra no, genera la misma lista.
% Y si las dos no están definidas, genera dos listas iguales.
%sameList(?L1, ?L2).
sameList([],[]).
sameList([X|Xs],[X|Ys]) :- sameList(Xs,Ys).

%%Predicados pedidos.

% 1) %esDeterministico(+Automata)
esDeterministico(Automata) :- not(noEsDeterministico(Automata)).

% noEsDeterministico(+Automata)
noEsDeterministico(Automata) :- transicionesDe(Automata,T),
								  member((O1,E1,D1), T),
								  member((O2,E2,D2),T),
								  O1 = O2, E1 = E2, D1 \= D2.


% 2) estados(+Automata, ?Estados)
estados(Automata, Estados) :- var(Estados), inicialDe(Automata, Si), 
				transicionesDe(Automata, Ts), origenesydstAutomata(Ts, ODs),
				finalesDe(Automata, Fs), append([Si|ODs], Fs, Es), sort(Es, Estados).

estados(Automata, Estados) :- nonvar(Estados), inicialDe(Automata, Si), 
				transicionesDe(Automata, Ts), origenesydstAutomata(Ts, ODs),
				finalesDe(Automata, Fs), append([Si|ODs], Fs, Es),
				forall(member(E, Es), member(E, Estados)).


% A partir de las transiciones de un automata, retorna una lista con todos los estados
% que participan de alguna transición.
% origenesydstAutomata(+Ts, ?L).
origenesydstAutomata([], []).
origenesydstAutomata([(O,_,D)|Ts], [O,D|ODs]) :- origenesydstAutomata(Ts, ODs).



% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)
esCamino(A, Si, Si, [Si]) :- estados(A, Estados), member(Si, Estados).
esCamino(A, Si, Sf, [Si|C]) :- last(C,Sf2), Sf = Sf2,
								transicionesDe(A,T), estanEn([Si|C], T).

%estanEn(-L1, +T)
estanEn([O,D], L2) :- member((O,_,D), L2), !.
estanEn([O,D|L1], L2) :- member((O,_,D), L2), estanEn([D|L1], L2). 									


% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% No, no es reversible. 
% Esto se debe a la forma de recorrer el camino para la verificación. No se tiene
% en cuenta el caso de generación del camino.
% El predicado genera los posibles caminos cuando no hay ciclo y luego se cuelga.
% Cuando hay ciclo, genera algunos caminos pero no todos, ya que se queda en el loop,
% y sigue infinitamente generando por la misma rama.

% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
caminoDeLongitud(A, 1, [Si], [], Si, Si) :- !, estados(A, Es), member(Si, Es). 			
caminoDeLongitud(A, N, C, Tags, Si, Sf):- N > 1, estados(A, Es), member(Si, Es), member(Sf, Es), transicionesDe(A,T), 
											Nm1 is N-1, crearCamino(Si, Sf, T, C, Nm1, Tags).


% Dado dos estados que serán el principio y el final del camino, genera los caminos de
% tamaño N con sus respectivas etiquetas. 
%crearCamino(?Si, ?Sf, +T, -Camino, +N, -Etiquetas).
crearCamino(X, Sf, T, [X,Sf], 1, [E]) :- !, member((X,E,Sf),T).
crearCamino(X, Sf, T, [X|C], N, [E|Etiquetas]) :- Nm1 is N-1, member((X,E,Y),T), crearCamino(Y, Sf, T, C, Nm1, Etiquetas). 


% Se utiliza Generate & Test.
% 6) alcanzable(+Automata, +Estado)
alcanzable(A, E) :- inicialDe(A, Si), 
					estados(A,Estados),	length(Estados, N), between(2, N, X),
					caminoDeLongitud(A, X, _, _, Si, E), !.

% 7) automataValido(+Automata)
automataValido(A) :- estados(A, Estados), finalesDe(A, Finales), subtract(Estados, Finales, EstadosNoFinales), 
						inicialDe(A, Inicial), subtract(Estados, [Inicial], EstadosNoIniciales), transicionesDe(A, Ts),
						todosTienenTransicionesSalientes(EstadosNoFinales, Ts), 
						todosAlcanzables(A, EstadosNoIniciales), 
						hayEstadoFinal(Finales), 
						noHayFinalesRepetidos(Finales),
						noHayTransicionesRep(Ts).

% Verifica que todos los estados tienen transiciones salientes exceptuando los finales, que pueden
% o no tenerlas.
%todosTienenTransicionesSalientes(+Estados, +Transiciones)
todosTienenTransicionesSalientes(Es, Ts) :- forall(member(E,Es), member((E, _, _), Ts)).

% Verifica que todos los estados son alcanzables desde el estado inicial.
%todosAlcanzables(+Automata, +Es)
todosAlcanzables(A, Es) :- forall(member(E, Es), alcanzable(A,E)). 

%Se comprueba que el automata tiene al menos un estado final.
%hayEstadoFinal(+Fs)
hayEstadoFinal(Fs) :- length(Fs, N), N > 0.

%Se chequea que no hay estados finales repetidos.
%noHayFinalesRepetidos(+Fs)
noHayFinalesRepetidos(Fs) :- sacarDup(Fs, FsSinDup), length(Fs, N), length(FsSinDup, N).

% Se fija que no haya transiciones repetidas.
%noHayTransicionesRep(+Ts)
noHayTransicionesRep(Ts) :- sacarDup(Ts, TsSinDup), length(Ts, N), length(TsSinDup, N).


%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% Se utiliza Generate & Test.
% 8) hayCiclo(+Automata)
hayCiclo(A) :- estados(A, Es), length(Es, M), P is M + 1, member(E, Es),
				between(2, P, N), caminoDeLongitud(A, N, _, _, E, E), !.


% Se utiliza Generate & Test.
% 9) reconoce(+Automata, ?Palabra)
reconoce(A, P) :- nonvar(P), length(P,N), inicialDe(A,Si), finalesDe(A,Sfs), 
					member(Sf,Sfs), Nm1 is N+1, caminoDeLongitud(A,Nm1,_,TmpP,Si,Sf), sameList(P,TmpP).

reconoce(A, P) :- var(P), not(hayCiclo(A)), inicialDe(A,Si), finalesDe(A,Sfs), 
					transicionesDe(A,T), length(T,N), Nm1 is N+1, between(1, Nm1, Tam), member(Sf,Sfs), caminoDeLongitud(A,Tam,_,P,Si,Sf).

reconoce(A, P) :- var(P), hayCiclo(A), inicialDe(A,Si), finalesDe(A,Sfs), 
					desde(1, N), member(Sf,Sfs), caminoDeLongitud(A,N,_,P,Si,Sf).


% Se utiliza Generate & Test.
% 10) PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(A, Palabra) :- nonvar(Palabra), reconoce(A,Palabra),  
								transicionesDe(A,T), length(T,N), Nm1 is N+1, inicialDe(A,Si), finalesDe(A,Sfs), 
								between(1,Nm1,Tam), member(Sf,Sfs), caminoDeLongitud(A,Tam,_,_,Si,Sf), !, Tam2 is Tam -1,
								length(Palabra,TamPal), Tam2 = TamPal.
palabraMasCorta(A, Palabra) :- var(Palabra), transicionesDe(A,T), length(T,N), Nm1 is N+1, inicialDe(A,Si), finalesDe(A,Sfs), 
							between(1,Nm1,Tam), member(Sf,Sfs), caminoDeLongitud(A,Tam,_,_,Si,Sf), !, Len is Tam - 1 ,
							reconoceAcotado(A, Palabra, Len).


% Se utiliza Generate & Test.
% Genera todas las palabras de longitud N de un automata dado.
%reconoceAcotado(+A, ?P, +N)
reconoceAcotado(A, P, N) :- length(P, N), reconoce(A, P).

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

test(16) :- forall(member(X, [1, 2, 3, 5, 6, 7, 9, 10]), (ejemplo(X, A), esDeterministico(A))).
test(17) :- not((member(X, [4, 8]), ejemplo(X, A), esDeterministico(A))).

tests :- forall(between(1, 17, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.


