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
%Nuevos ejemplos
ejemplo(11, a(s1,[sf],[(s1,t,s2),(s1,l,s2),(s2,a,s3),(s3,s,sf),(s2,o,s4),(s4,c,s5),(s4,k,s5),(s5,o,sf)])). 
ejemplo(12, a(s1,[sf],[(s1,a1,s2),(s1,a1,s3),(s2,a2,sf),(s3,a2,sf)])). 
ejemplo(13, a(si, [sf,s3], [(si,a,s1),(s1,a,s11),(s11,b,s10),(s10,c,sf),(si,p,s3)])).
ejemplo(14, a(si, [si], [(si,c,s1),(s1,i,s2),(s2,c,s3),(s3,l,s4),(s4,o,si)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2), (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.
%Nuevos ejemplos
ejemploMalo(8, a(si,[s4,s4],[(s1,a,s2),(s3,b,s4)])). %s1, s2 y s3 no son alcanzables y si no tiene transiciones salientes
ejemploMalo(9, a(si,[],[(si,o,s1),(s1,h,si)])).
ejemploMalo(10, a(si,[],[(si,a,s1),(s1,h,si),(si,a,s1)])).




%%Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

% Auxiliar dada en clase
% desde(+X, -Y).
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.

%Funciones auxiliares

% Se eliminan los elementos duplicados de la primer lista L1.
% sacarDup(+L1, -L2).
sacarDup([],[]).
sacarDup([X|L],L2) :- member(X,L), !, sacarDup(L,L2).
sacarDup([X|L],[X|L2]) :- not(member(X,L)), sacarDup(L,L2).

% Si ambas están definidas, retorna true si las dos son iguales.
% Si una está instanciada y la otra no, genera la misma lista.
% Y si las dos no están definidas, genera dos listas iguales.
% sameList(?L1, ?L2).
sameList([],[]).
sameList([X|Xs],[X|Ys]) :- sameList(Xs,Ys).

%%Predicados pedidos.


% Se realizó primero la función noEsDeterministico y a partir de la negación 
% de la misma se determina si el autómata lo es.
% En noEsDeterministico da True cuando encuentra dos transiciones con el mismo
% origen y la misma etiqueta.
% 1) %esDeterministico(+Automata)
esDeterministico(Automata) :- not(noEsDeterministico(Automata)).

% noEsDeterministico(+Automata)
noEsDeterministico(Automata) :- transicionesDe(Automata,T),
								  member((O1,E1,D1), T),
								  member((O2,E2,D2),T),
								  O1 = O2, E1 = E2, D1 \= D2.


% Tanto si Estados es variable o no, se genera una lista con el estado inicial,
% con los estados de origen y destinos de las transicones del autómata y con 
% los estados finales. Para el primer caso se usa "sort" que ordena la lista
% de los estados alfabéticamente y además quita los repetidos. Para el segundo
% caso, cuando Estados no es variable, se verifica que todos los estados de la
% lista generada estén en Estados.
% 2) estados(+Automata, ?Estados)
estados(Automata, Estados) :- var(Estados), inicialDe(Automata, Si), 
				transicionesDe(Automata, Transiciones), origenesydstAutomata(Transiciones, ODs),
				finalesDe(Automata, Finales), append([Si|ODs], Finales, EstadosConRepetidos), sort(EstadosConRepetidos, Estados).

estados(Automata, Estados) :- nonvar(Estados), inicialDe(Automata, Si), 
				transicionesDe(Automata, Transiciones), origenesydstAutomata(Transiciones, ODs),
				finalesDe(Automata, Finales), append([Si|ODs], Finales, EstadosConRepetidos),
				forall(member(E, EstadosConRepetidos), member(E, Estados)).


% A partir de las transiciones de un automata, retorna una lista con todos los estados
% que participan de alguna transición.
% origenesydstAutomata(+Ts, ?L).
origenesydstAutomata([], []).
origenesydstAutomata([(O,_,D)|Transiciones], [O,D|ODs]) :- origenesydstAutomata(Transiciones, ODs).



% Si el camino es de de longitud uno, el estado inicial y final deben ser el mismo,
% y únicamente se verifica que pertenezca a los estados del autómata.
% Cuando el camino tiene una longitud mayor o igual que dos, se comprueba que el 
% estado inicial sea el mismo que el primero del camino,y que el final sea el último 
% de la lista que representa el camino. Luego, se chequea que dos estados continuos 
% del caminos formen parte de alguna de las transiciones del autómata. 
% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)
esCamino(Automata, Si, Si, [Si]) :- estados(Automata, Estados), member(Si, Estados).
esCamino(Automata, Si, Sf, [Si|C]) :- last(C,Sf2), Sf = Sf2,
								transicionesDe(Automata, Transiciones), estanEn([Si|C], Transiciones).

%estanEn(-L1, +T)
estanEn([O,D], L2) :- member((O,_,D), L2), !.
estanEn([O,D|L1], L2) :- member((O,_,D), L2), !, estanEn([D|L1], L2). 									


% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% No, no es reversible. 
% Esto se debe a la forma de recorrer el camino para la verificación. No se tiene
% en cuenta el caso de generación del camino.
% El predicado genera los posibles caminos cuando no hay ciclo y luego se cuelga.
% Cuando hay ciclo, genera algunos caminos pero no todos, ya que se queda en el loop,
% y sigue infinitamente generando por la misma rama.


% Si el camino es de longitud uno, se comprueba que el estado que forma el camino, sea
% parte de los estados del automata. Si la longitud del camino es mayor que uno, se crea
% un camino desde el estado S1 hasta el S2, a partir de las transiciones del autómata.
% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
caminoDeLongitud(Automata, 1, [Si], [], Si, Si) :- !, estados(Automata, Estados), member(Si, Estados). 			
caminoDeLongitud(Automata, N, Camino, Etiquetas, Si, Sf):- N > 1, estados(Automata, Estados), member(Si, Estados), member(Sf, Estados), 
													transicionesDe(Automata,Transiciones), 
											Nm1 is N-1, crearCaminoConEtiquetas(Si, Sf, Transiciones, Camino, Nm1, Etiquetas).


% Dado dos estados que serán el principio y el final del camino, genera los caminos de
% tamaño N con sus respectivas etiquetas. 
%crearCaminoConEtiquetas(?Si, ?Sf, +Transiciones, -Camino, +N, -Etiquetas).
crearCaminoConEtiquetas(X, Sf, Transiciones, [X,Sf], 1, [Etiqueta]) :- !, member((X,Etiqueta,Sf),Transiciones).
crearCaminoConEtiquetas(X, Sf, Transiciones, [X|C], N, [E|Etiquetas]) :- Nm1 is N-1, member((X,E,Y),Transiciones), 
													crearCaminoConEtiquetas(Y, Sf, Transiciones, C, Nm1, Etiquetas). 


% Se utiliza Generate & Test.
% Si hay un camino desde el estado inicial hasta el estado dado con alguna longitud 
% entre 2 y la cantidad de estados más uno, entonces la función da True.
% 6) alcanzable(+Automata, +Estado)
alcanzable(Automata, Estado) :- inicialDe(Automata, Inicial), 
					estados(Automata,Estados),	length(Estados, N), Nm1 is N+1, between(2, Nm1, M),
					caminoDeLongitud(Automata, M, _, _, Inicial, Estado), !.


% Se seleccionan los estados finales del autómata, los no finales, el inicial, los no iniciales y 
% sus transiciones. Luego se verifican que se cumplan las condiciones para que un autómata sea
% válidad. Éstas son explicadas más abajo.
% 7) automataValido(+Automata)
automataValido(Automata) :- estados(Automata, Estados), finalesDe(Automata, Finales), subtract(Estados, Finales, EstadosNoFinales), 
						inicialDe(Automata, Inicial), subtract(Estados, [Inicial], EstadosNoIniciales), transicionesDe(Automata, Transiciones),
						todosTienenTransicionesSalientes(EstadosNoFinales, Transiciones), 
						todosAlcanzables(Automata, EstadosNoIniciales), 
						hayEstadoFinal(Finales), 
						noHayFinalesRepetidos(Finales),
						noHayTransicionesRep(Transiciones).

% Verifica que todos los estados tienen transiciones salientes exceptuando los finales, que pueden
% o no tenerlas.
%todosTienenTransicionesSalientes(+EstadosNoFinales, +Transiciones)
todosTienenTransicionesSalientes(EstadosNoFinales, Transiciones) :- forall(member(Estado,EstadosNoFinales), member((Estado, _, _), Transiciones)).

% Verifica que todos los estados son alcanzables desde el estado inicial.
%todosAlcanzables(+Automata, +EstadosNoIniciales)
todosAlcanzables(Automata, EstadosNoIniciales) :- forall(member(Estado, EstadosNoIniciales), alcanzable(Automata,Estado)). 

%Se comprueba que el automata tiene al menos un estado final.
%hayEstadoFinal(+Fs)
hayEstadoFinal(Finales) :- length(Finales, N), N > 0.

%Se chequea que no hay estados finales repetidos.
%noHayFinalesRepetidos(+Fs)
noHayFinalesRepetidos(Finales) :- sacarDup(Finales, FinalesSinDup), length(Finales, N), length(FinalesSinDup, N).

% Se fija que no haya transiciones repetidas.
%noHayTransicionesRep(+Ts)
noHayTransicionesRep(Transiciones) :- sacarDup(Transiciones, TransicionesSinDup), length(Transiciones, N), length(TransicionesSinDup, N).


%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% Se utiliza Generate & Test.
% Si se encuentra un camino que empiece y termine en el mismo estado, con alguna longitud 
% entre 2 y la cantidad de estados más uno, hay un ciclo y la función da True.
% 8) hayCiclo(+Automata)
hayCiclo(Automata) :- estados(Automata, Estados), length(Estados, M), P is M + 1, member(Estado, Estados),
				between(2, P, N), caminoDeLongitud(Automata, N, _, _, Estado, Estado), !.


% Se utiliza Generate & Test.
% Si la Palabra que se desea reconocer en el Autómata está definida, entonces se buscan todas las palabras que
% se pueden conformar en ese mismo Autómata con la misma longitud a la Palabra dada. Luego, se comparan ambas 
% verificando si alguna resulta ser idéntica. Si la palabra no está instanciada, se distingue el caso de que
% el Autómata posee ciclos o no. En el primer caso, se generan entonces todas las palabras de longitud 1 hasta
% infinito. Caso contrario, se generan todas las palabras que se pueden conformar con longitud 1 hasta 
% la cantidad de Transiciones que posea el Autómata. 
% 9) reconoce(+Automata, ?Palabra)
reconoce(Automata, Palabra) :- nonvar(Palabra), length(Palabra,N), inicialDe(Automata,Si), finalesDe(Automata,Finales), 
					member(Sf,Finales), Nm1 is N+1, caminoDeLongitud(Automata,Nm1,_,TmpPalabra,Si,Sf), sameList(Palabra,TmpPalabra).

reconoce(Automata, Palabra) :- var(Palabra), not(hayCiclo(Automata)), inicialDe(Automata,Si), finalesDe(Automata,Finales), 
					transicionesDe(Automata,Transiciones), length(Transiciones,N), Nm1 is N+1, between(1, Nm1, Tam), member(Sf,Finales), 
					caminoDeLongitud(Automata,Tam,_,Palabra,Si,Sf).

reconoce(Automata, Palabra) :- var(Palabra), hayCiclo(Automata), inicialDe(Automata,Si), finalesDe(Automata,Finales), 
					desde(1, N), member(Sf,Finales), caminoDeLongitud(Automata,N,_,Palabra,Si,Sf).


% Se utiliza Generate & Test.
% Si Palabra está instanciada, se verifica que el autómata la reconozca y luego que su longitud sea
% la más corta de los posibles caminos desde el estado inicial a algunos de los finales.
% En el caso que Palabra sea una variable, se busca cual es la longitud más pequeña de los caminos del autómata
% que van desde el estado inicial a un estado final. Luego, con reconoceAcotado (explicado debajo), se generan 
% todas la palabras que reconoce el autómata con la longitud hallada.
% 10) PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(Automata, Palabra) :- nonvar(Palabra), reconoce(Automata,Palabra),  
								transicionesDe(Automata,Transiciones), length(Transiciones,N), Nm1 is N+1, inicialDe(Automata,Si), 
								finalesDe(Automata,Sfs), 
								between(1,Nm1,Tam), member(Sf,Sfs), caminoDeLongitud(Automata,Tam,_,_,Si,Sf), !, Tam2 is Tam -1,
								length(Palabra,TamPal), Tam2 = TamPal.
palabraMasCorta(Automata, Palabra) :- var(Palabra), transicionesDe(Automata,Transiciones), length(Transiciones,N), Nm1 is N+1, 
									inicialDe(Automata,Si), finalesDe(Automata,Sfs), 
							between(1,Nm1,Tam), member(Sf,Sfs), caminoDeLongitud(Automata,Tam,_,_,Si,Sf), !, Len is Tam - 1 ,
							reconoceAcotado(Automata, Palabra, Len).


% Se utiliza Generate & Test.
% Genera todas las palabras de longitud N de un automata dado.
%reconoceAcotado(+Automata, ?Palabra, +N)
reconoceAcotado(Automata, Palabra, N) :- length(Palabra, N), reconoce(Automata, Palabra).

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


%Test para ejercicio 1.
test(16) :- forall(member(X, [1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14]), (ejemplo(X, A), esDeterministico(A))).
test(17) :- not((member(X, [4, 8, 12]), ejemplo(X, A), esDeterministico(A))).

%Test para ejercicio 2.
test(18) :- ejemplo(11,A), findall(Es, estados(A,Es), [[s1,s2,s3,s4,s5,sf]]).
test(19) :- ejemplo(12,A), findall(Es, estados(A,Es), [[s1,s2,s3,sf]]).
test(20) :- not((ejemplo(9,A), estados(A,[s1]))).
test(21) :- ejemplo(13,A), estados(A,[s1,s10,s11,s3,sf,si]).

%Test para ejercicio 3.
test(22) :- not((ejemplo(5,A), esCamino(A, s1, s2, [s1,s2,s3,s2]))).
test(22) :- not((ejemplo(5,A), esCamino(A, s1, s5, [s1,s2,s3,s5]))).
test(23) :- ejemplo(4,A), esCamino(A, s1, s3, [s1,s1,s1,s1,s1,s1,s3]).
test(24) :- ejemplo(11,A), findall(X, esCamino(A, s1, X, [s1,s2,s3,sf]), [(sf)]).
test(25) :- ejemplo(5,A), findall(X, esCamino(A, X, s2, [s1,s1,s2]), [(s1)]).
test(26) :- ejemplo(5,A), findall(X, esCamino(A, X, s3, [s1,s1,s2]), []).

%Test para ejercicio 5.
%test(NUMERO) :- ejemplo(4,A), estados(A,Es), caminoDeLongitud(A, 1, Camino, Etiquetas, Si, Sf), member(Si, Es), Sf is Si, Camino = [Si], lenght(Etiquetas,0).
%ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).

test(27) :- ejemplo(8,A), findall(Camino, caminoDeLongitud(A,5,Camino,_,s3,s1), [[s3,s1,s2,s3,s1],[s3,s1,s2,s3,s1]]),
			findall(Etiquetas, caminoDeLongitud(A,5,_,Etiquetas,s3,s1), [[a,a,a,a],[a,a,b,a]]).
test(28) :- not((ejemplo(9,A), caminoDeLongitud(A,5,_,_,s1,s2))).
test(29) :- not((ejemplo(12,A), caminoDeLongitud(A,3,_,_,s2,s3))).


%Test para ejercicio 6.
test(30) :- ejemplo(2,A), alcanzable(A,si).
test(31) :- ejemplo(11,A), alcanzable(A,s5).
test(32) :- ejemplo(10,A), alcanzable(A,s7).
test(33) :- not((ejemploMalo(2,A), alcanzable(A,sf))).
test(34) :- not((ejemploMalo(3,A), alcanzable(A,s2))).
test(35) :- not((ejemploMalo(4,A), alcanzable(A,s2))).

%Test para ejercicio 7.
%No agrego nuevos, ya que el dado por la cátedra verifica todos los ejemplos, y como ya creamos nuevos...

%Test para ejercicio 8.
test(36) :- ejemplo(14, A), hayCiclo(A).
test(37) :- not((member(X, [11, 12, 13]), ejemplo(X, A), hayCiclo(A))).

%Test para ejercicio 9.
test(38) :- ejemplo(14,A), reconoce(A, [c,i,c,l,o,c,i,c,l,o]).
test(39) :- not((ejemplo(14,A), reconoce(A, [c,i,c,l,o,c,i,c,l,o,c,i]))).
test(40) :- ejemplo(11,A), findall(Palabra, reconoce(A,Palabra), [[t,a,s],[l,a,s],[t,o,c,o],[t,o,k,o],[l,o,c,o],[l,o,k,o]]).
test(41) :- ejemplo(11,A), reconoce(A, [l,o,X,o]), member([X],[[k],[c]]). %¿Como escribo que el X es igual a k o a c?

%Test para ejercicio 10.
test(42):- ejemplo(11,A), palabraMasCorta(A,[t,a,s]).
test(43):- not((ejemplo(14,A), palabraMasCorta(A,[c,i,c,l,o,c,i,c,l,o]))). 
test(44):- ejemplo(11,A), findall(Palabra, palabraMasCorta(A,Palabra), [[t,a,s],[l,a,s]]).
test(45):- ejemplo(10,A), findall(Palabra, palabraMasCorta(A,Palabra), [[p,r,o,l,o,g]]).


tests :- forall(between(1, 45, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.


%ejemplo(11, a(s1,[],[(),(),(),(),()])) 