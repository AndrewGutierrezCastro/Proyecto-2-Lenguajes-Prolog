/*
 * solve_hill_climb(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */

% Si el Estado actual es un estado final, no hay que moverse.
solve_hill_climb(State,_,[]) :-
    final_state(State).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 * Las movidas son intentadas en el orden establecido por la heur�stica
 * que eval�a la "bondad" de los estados que se alcanzan para cada movida.
 */
solve_hill_climb(State,History,[Move|Moves]) :-
    hill_climb(State,Move),      % generar una nueva Movida en el orden heur�stico
    update(State,Move,State1),   % calcula nuevo estado usando Movida
    legal(State1),               % nuevo estado debe ser legal
    not(member(State1,History)), % debe ser primera vez que se llega al nuevo estado
    solve_hill_climb(State1,[State1|History],Moves).   % continuar a partir de nuevo estado

/*
 *  A partir de un Estado devuelve una Movida.
 *  Primero genera todas las movidas, luego las eval�a usando una heur�stica,
 *  y finalmente las va usando en orden decreciente de la evaluaci�n obtenida.
 */
hill_climb(State,Move) :-
    findall(M,move(State,M),Moves),         % Encuentra todas las movidas posibles
    evaluate_and_order(Moves,State,[],MVs), % Eval�a con la heur�stica todas las movidas y las ordena.
    member((Move,_),MVs).                   % Escoge movidas en orden de heur�stica


/*
 * evaluate_and_order(Movidas,Estado,AcumuladorParcial,MovidasOrdenadas)
 *   Todas las Movidas del Estado actual
 *   son evaluadas y almacenadas en orden en MovidasOrdenadas
 *   Acumulador es donde se van acumulando parcialmente las movidas evaluadas.
 */

% Caso: procesar la primera movida y continuar recursivamente
evaluate_and_order([Move|Moves],State,MVs,OrderedMVs) :-
    update(State,Move,State1),         % obtiene nuevo estado usando movida
    value(State1,Value),               % calcula el valor heur�sico del nuevo estado
    insertPair((Move,Value),MVs,MVs1), % inserta en orden el par (movida,valor) en lista de movidas
    evaluate_and_order(Moves,State,MVs1,OrderedMVs).  % procesa recursivamente el resto de movidas
    
% Caso base: no hay m�s movidas que evaluar. Se retorna el acumulador como resultado.
evaluate_and_order([],_,MVs,MVs).

insertPair(MV,[],[MV]).
insertPair((M,V),[(M1,V1)|MVs],[(M,V),(M1,V1)|MVs]) :-
    V >= V1.
insertPair((M,V),[(M1,V1)|MVs],[(M1,V1)|MVs1]) :-
    V < V1,insertPair((M,V),MVs,MVs1).


/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_hill_climb(Problem,Moves) :-
   initial_state(Problem,State),           % obtener un Estado inicial dado Problema
   solve_hill_climb(State,[State],Moves).  % inicia resoluci�n desde Estado



% === Relaciones que definen el problema zgm     === %
% === Son las mismas incluidas en depth-first.pl === %

initial_state(pnt,pnt(izq,[alberto,beatriz,carlos,dora,emilio],[])).

final_state(pnt(der,[],[alberto,beatriz,carlos,dora,emilio])).

move(pnt(izq,I,_),Carga):-member(Carga,I).
move(pnt(der,_,D),Carga):-member(Carga,D).
move(pnt(_,_,_),solo).

update(pnt(B,I,D),Carga,pnt(B1,I1,D1)):-
update_Bote(B,B1),
update_margenes(Carga,B,I,D,I1,D1, personas_a_la_vez, 0).

update_Bote(izq,der).
update_Bote(der,izq).

update_margenes(solo,_,I,D,I,D,_,_).

update_margenes(Carga,izq,I,D,I1,D1,N,M):-
    M < N,
    M1 is M + 1,
    select(Carga,I,I1),
    insert(Carga,D,D1),
    update_margenes(Carga,izq,I,D,I1,D1,N,M1).

update_margenes(Carga,der,I,D,I1,D1,N,M):-
    M < N,
    M1 is M + 1,
    select(Carga,D,D1),
    insert(Carga,I,I1),
    update_margenes(Carga,der,I,D,I1,D1,N,M1).

/*
 * insert(ElementoInsertado, ListaVieja, ListaNueva)
 *
 * Inserta en orden una de las cosas en una lista.
 * La relaci�n precedes/2 establece el orden de las cosas: z < g < m.
 */
insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,Y).   % Elemento va al inicio
insert(X,[Y|Ys],[Y|Zs]):-precedes(Y,X),insert(X,Ys,Zs).  % Insertar m�s adentro.
insert(X,[],[X]).                           % Insertar como �nico elemento.


/*
 * select(Elemento, ListaQueContieneElemento, ListaSinElemento)
 *
 * Extrae no determisticamente un elemento de una lista que lo contiene
 * y obtiene la lista sin ese elemento.
 */
select(X,[X|Xs],Xs).                          % Extrae primer elemento.
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     % Extrae elemento de m�s adentro.



% Caso no determin�stico
% precedes(zorra,_).
% precedes(_,maiz).

% Caso determin�stico
precedes(X, Y):- 
    persona(X, V),
    persona(Y, V1),
    V =< V1,
    X \= Y.


legal(pnt(izq,_,D)):-not(ilegal(D)).
legal(pnt(der,I,_)):-not(ilegal(I)).

ilegal(L):-member(zorra,L),member(gallina,L).
ilegal(L):-member(gallina,L),member(maiz,L).

% === Fin de las relaciones usadas por depth-first.pl ==%

% === Relaci�n adicional requerida por hill-climb para resolver zgm. === %
% === Value/2 es una heur�sica que da valores m�s altos conforme     === %
% === haya m�s cosas en la rivera derecha.                           === %

value(pnt(_,_,[]),0).
value(pnt(_,_,[_]),1).
value(pnt(_,_,[_,_]),2).
value(pnt(_,_,[_,_,_]),3).
value(pnt(_,_,[_,_,_,_]),4).
value(pnt(_,_,[_,_,_,_,_]),5).

persona(alberto, 1).
persona(beatriz, 2).
persona(carlos, 5).
persona(dora, 10).
persona(emilio, 15).

personas_a_la_vez(2).

/*Selecciona de la lista las persona mas rapida en cruzarlo*/
personaRapida(X, [X|XS], Y):-
    min(X,Y, V2).
/*Selecciona la persona con el minimo tiempo de 
    cruce del puente*/
min(X,Y,V2):-
    persona(X,X1),
    persona(Y,Y1),
    X1 >= Y1,
    V2 = Y.

min(X,Y,V2):-
    persona(X,X1),
    persona(Y,Y1),
    X1 < Y1,
    V2 = X.

max(X,Y,V2):-
    persona(X,X1),
    persona(Y,Y1),
    X1 >= Y1,
    V2 = X.

max(X,Y,V2):-
    persona(X,X1),
    persona(Y,Y1),
    X1 < Y1,
    V2 = Y.


/*Selecciona el maximo de una lista de personas*/
selectMax(P,[X|XS]):-
    selectMax(P1, XS),
    max(X,P1,P).
selectMax(P,[X]):-
    P = X.

/*Selecciona las n personas mas lentas de la lista X de perosnas*/
npersonas_mas_lentas(N,[],A,R).

npersonas_mas_lentas(N,X,A,R):-
    N > 0,
    selectMax(P,X),   %seleccionar la personas con max duracion
    select(P, X, L),  %quitar esa persona de la lista, L es la nueva lista sin el elemento P
    insert(P, A, A1), %R1 es la lista R con el valor P insertado
    N1 is N - 1, 
    npersonas_mas_lentas(N1, L, A1, R).

npersonas_mas_lentas(_,_,R,R).