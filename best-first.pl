/*
 * solve_best/3 usa b�squeda por anchura
 *   El primer elemento es una lista de puntos que deben ser explorados.
 *   Esta lista es conocida como la frontera (Frontier).
 *
 *   El segundo elemento de solve_best/3 es el historial de los puntos que
 *   que ya han sido explorados y que no deben ser explorados de nuevo.
 *
 *   El tercer elemento de solve_best/3 la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de Point.
 *
 *   Cada punto de exploraci�n en la frontera tiene la forma
 *       state(Estado, Ruta, Valor)
 *   D�nde
 *       Estado es la descripci�n del estado en que se encuentra la
 *              resoluci�n del problema independientemente del mecanismo
 *              de b�squeda. Esto es, es el mismo tipo de estado usado en
 *              b�squedas como depth-first o hill-climbing.
 *       Ruta es la secuencia de movidas que se requieren para llegar del
 *              estado incial a Estado.
 *       Valor es el estimado heur�stico de cu�n bueno es este estado para
 *              alcanzar el estado final.
 *   La lista de puntos se ecuentra ordenada en forma decreciente por Valor.
 */

 /*
  * Si el mejor punto en la frontera corresponde a un estado final,
  * no hay que buscar m�s.
  * Se obtiene la secuencia de movidas que llevan del estado inicial a este
  * estado final simplemente revirtiendo el orden de movidas encontradas en
  * la ruta correspondiente a este estado.
  */
testing(Moves1):-
  initial_state(puente,ESTADO), 
  value(ESTADO, Value),
  test([punto(estado(izq,[alberto,carlos],[beatriz],18),[],2)],[estado(izq,[alberto,carlos],[beatriz],18)],Moves1).


test([punto(State,Path,_)|History],_,Path):-
  /*write(State),write(" Path"),
  write(Path),write(" History"),
  write(History),nl,*/
  final_state(State).



test([punto(State,Path,_)|Frontier],History,Moves1):-
  findall(M,move(State,M),Moves), 
  updates(Moves,Path, State, Estados),
  legals(Estados, Estados1),
  sort(Estados1, Estados2), 
  evaluates(Estados2, Values),
  
  inserts(Values,Frontier,Frontier1),
  test(Frontier1,[State|History],Moves1).

/*
 * Inicializa un problema y lo resuelve.
 *   Problem: nombre del problema.
 *   Moves: movidas requeridas para resolver el problema.
 */
test_best_search(Problem,Moves) :-
  initial_state(Problem,State),   % obtener un Estado inicial dado Problema
  value(State,Value),             % calcula el valor heur�stico del estado incial
  solve_best([punto(State,[],Value)],[State],Moves). % inicializa frontera e historial,
                                                     % inicia resoluci�n


solve_best([punto(State,Path,_)|_],_,Moves) :-
    final_state(State),reverse(Path,Moves).

/*
 * Si el mejor punto en la frontera no corresponde a un estado final:
 *     * se generan todas las movidas posibles a partir del estado de ese punto
 *     * se obtienen los nuevos estados que se alcanzar�an con esas movidas
 *     * se calcular�an los valores heur�sticos de los nuevos estados
 *     * se introducen los nuevos estados como nuevo puntos en la frontera
 *     * se elimina el mejor punto de la frontera y se incluye en el historial.
 */
 
solve_best([punto(State,Path,_)|Frontier],History,FinalPath) :-
    findall(M,move(State,M),Moves),     % obtiene movidas del mejor estado
    updates(Moves,Path,State,States),   % obtiene los nuevos estados usando movidas
    legals(States,States1),             % escoge los nuevos estados que son legales
    news(States1,History,States2),      % elimina nuevos estados ya incluidos en historial
    evaluates(States2,Values),          % calcula valores heur�sticos de los nuevos estados
    inserts(Values,Frontier,Frontier1), % inserta en orden los nuevos puntos en la frontera
    solve_best(Frontier1,[State|History],FinalPath). % continuar a partir de nueva frontera


/*
 * updates(Moves,Path,State,States)
 *   States es la lista de posibles estados accesables a partir
 *   de State usando la lista de posibles movidas (Moves).
 *   Path es la ruta de movidas que llevan del estado inicial a State.
 *   Las rutas de los nuevos estados se agregan al inicio su respectiva movida
 *   a la ruta de State.
 *   States es una lista de pares (NuevoEstado, NuevaRuta).
 */

updates( [M|Ms], Path, S, [(S1,[M|Path])|Ss]) :-
    update(S,M,S1),         % obtiene el estado al que se llega por una movida
    updates(Ms,Path,S,Ss).  % procesa recursivamente las siguientes movidas
updates([],_,_,[]).


/*
 * legasls(States,States1)
 *   States1 es el subconjunto de la lista State que son estados legales.
 *   Maneja pares (Estado,Ruta).
 */

% el primer estado es legal, incluirlo en la nueva lista
legals([(S,P)|States],[(S,P)|States1]) :-
    legal(S),
    legals(States,States1).
    
% primer estado ilegal, excluirlo de la nueva lista
legals([(S,_)|States],States1) :-
    not(legal(S)),
    legals(States,States1).
    
legals([],[]).


/*
 * news(States,History,States1)
 *   States1 es el subconjunto de la lista States que consiste de estados
 *   que no aparecen en el historial.
 *   Maneja pares (Estado,Ruta).
 */

% primer estado ya aparece en historial, excluirlo de la nueva lista
news([(S,_)|States],History,States1) :-
    member(S,History),
    news(States,History,States1).

% primer estado no aparece en historial, incluirlo en nueva lista
news([(S,P)|States],History,[(S,P)|States1]) :-
    not(member(S,History)),
    news(States,History,States1).
    
news([],_,[]).


/*
 * evaluates(States,Values)
 *   Calcula el valor heur�stico de los estados en la lista States.
 *   Values is la lista resultante con los estados junto con sus valores.
 *   La lista State consiste de pares (Estado,Ruta).
 *   La lista Values consiste de estructuras punto(Estado,Ruta,Valor).
 */

evaluates([(S,P)|States],[punto(S,P,V)|Values]) :-
    value(S,V),                % calcula valor heur�stico del estado S
    evaluates(States,Values).  % procesa resto de estados
evaluates([],[]).


/*
 * inserts(Points,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar una lista de puntos (Points)
 *   en una frontera anterior (Frontier).
 *   Los puntos son insertados preservando el orden descendente en el
 *   valor heur�stico.
 */

inserts([Punto|Puntos],Frontier,Frontier1) :-
    insertPoint(Punto,Frontier,Frontier0),  % inserta primer punto
    inserts(Puntos,Frontier0,Frontier1).    % recursivamente inserta los dem�s puntos
inserts([],Frontier,Frontier).


/*
 * insertPoint(Point,Frontier,Frontier1)
 *   Frontier1 es el resultado de insertar el punto Points en
 *   su posici�n correcta dentro de Frontier de acuerdo con el orden
 *   del valor heur�stico.
 *
 */
insertPoint(Point,[],[Point]).

% nuevo punto es mejor que el primero de la frontera,
% va de primero en nueva frontera
insertPoint(Point,[Point1|Points],[Point1,Point|Points]) :-
    less_than(Point1,Point).

% nuevo punto es igual al primero de la frontera,
% nuevo punto se ignora y se deja la frontera sin cambios
insertPoint(Point,[Point1|Points],[Point|Points]) :-
    equals(Point,Point1).

% nuevo punto es peor que el primero de la frontera,
% el primero de la frontera de deja en el primer lugar y
% el nuevo punto se inserta recursivamente dentro del resto de la frontera
insertPoint(Point,[Point1|Points],[Point1|Points1]) :-
    less_than(Point,Point1),
    insertPoint(Point,Points,Points1).

% nuevo punto no es igual a primero de la frontera pero tiene
% el mismo valor heur�stico, se pone al nuevo punto como primero
% en la nueva frontera
insertPoint(Point,[Point1|Points],[Point,Point1|Points]) :-
    same(Point,Point1).


/*
 * relaciones de comparaci�n de puntos
 *
 * no se puede dar el caso de que dos puntos tengan el mismo estado
 * pero diferente valor
 */

% dos puntos son iguales si contienen el mismo estado y tienen el mismo valor;
% se ignoran las rutas: no importa c�mo se haya llegado al mismo estado
equals(punto(S,_,V),punto(S,_,V)).

% un punto es menor que otro, si contienen diferentes estados y si el
% valor del primero es menor que el valor del segundo;
% las rutas son ignoradas
less_than(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 < V2.

% dos puntos tienen el mismo valor si contienen diferentes estados
% y si sus valores son iguales
same(punto(S1,_,V1),punto(S2,_,V2)) :- S1 \= S2, V1 = V2.


initial_state(puente,estado(izq,[alberto,beatriz,carlos,dora,emilio],[],N)):-
    tiempo_cruzar(N).

final_state(estado(der,[],[alberto,beatriz,carlos,dora,emilio],_)).

value(estado(_,[],_,N), N).


value(estado(_,LstIzq,LstDer,N),Value):-
    personaRapida(P, LstIzq),
    selectMax(PerLenta, LstIzq),
    persona(P, ValPerRapid),
    persona(PerLenta, ValPerLen),
    length(LstIzq, LargoListaIzq),
    Value is (ValPerLen - ValPerRapid) - LargoListaIzq.

legal(estado(_,_,_,N)):-
    N >= 0.

update(estado(LADO,LstIzq,LstDer,N), Movida, NuevoEstado):-
    selectMax(PersonaMasLenta,Movida),
    persona(PersonaMasLenta, ValPerMasLenta),
    N1 is N - ValPerMasLenta,
    realizarMovimiento(Movida, estado(LADO,LstIzq,LstDer,N1),ESTADO2), %hace el movimiento 
    cruzarPuente(ESTADO2, NuevoEstado).
    

move(estado(der,LstIzq,LstDer,N),AMOVER):-     %Devolver en AMOVER la lista de 
    personaRapida(A, LstDer),
    AMOVER = [A].        

  move(estado(_,[],LstDer,_),[]).

move(estado(izq,LstIzq,LstDer,_),AMOVER):-
    findall(MOVIDAS, selectPersonas(MOVIDAS, LstIzq), SOLREPS),
    sort(SOLREPS,SOLsinREP),
    member(AMOVER, SOLsinREP).
    

%move(estado(izq,LstIzq,LstDer,_), AMOVER):-     %Devolver en AMOVER la lista de 
    /*Si se quiere mover de la izquierda a la derecha
    se revisa si a la izquierda ya hay una persona rapida
    para pasar a npersonas lentas y asi reducir el tiempo total*/
    /*LstDer \= [],                                           %debe haber al menos una persona en la lista derecha
    personaRapida(PersonaIzq, LstIzq),
    personaRapida(PersonaDer, LstDer),
    persona(PersonaIzq, PerIzqNum),
    persona(PersonaDer, PerDerNum),
    PerDerNum < PerIzqNum,
    personas_a_la_vez(N),              
    npersonas_lentas(N ,LstIzq, RESTO, AMOVER).*/     %elementos a mover de la lista lstIzq
                                                    %elementos a mover de la lista lstIzq                                  
                                                     %elementos a mover de la lista LstDer

/*move(estado(izq,LstIzq,LstDer,_), AMOVER):-
    length(LstIzq, LstLength),
    LstLength > 2,
    select(PersonaRapida, LstIzq, LstIzqNueva),
    findall(MOVIDAS, selectPersonas(MOVIDAS, LstIzqNueva), SOLREPS),
    sort(SOLREPS,SOLsinREP),
    member(AMOVER, SOLsinREP). */
/*
move(estado(izq,LstIzq,LstDer,_),AMOVER):-     %Devolver en AMOVER la lista de individuos a cruzar
    personaRapida(A, LstIzq),                               %
    npersonas_mas_lentas(LstIzq, RESTO, MOVER),
    AMOVER = [A|MOVER].  
*/

realizarMovimiento(AMOVER, estado(izq, LstIzq, LstDer, N),NEWESTADO):-
    selectList(LstIzq,AMOVER,LstIzqRESTO),
    insertList(AMOVER,LstDer,LstDerRESULTADO),
    NEWESTADO = estado(izq, LstIzqRESTO, LstDerRESULTADO, N).

realizarMovimiento(AMOVER, estado(der, LstIzq, LstDer, N),NEWESTADO):-
    selectList(LstDer,AMOVER,LstDerRESTO),
    insertList(AMOVER,LstIzq, LstIzqRESULTADO),
    NEWESTADO = estado(der, LstIzqRESULTADO, LstDerRESTO, N).

cruzarPuente(estado(izq,LstIzq,LstDer, N), ESTADO2):-
    ESTADO2 = estado(der, LstIzq, LstDer, N).
cruzarPuente(estado(der,LstIzq,LstDer, N), ESTADO2):-
    ESTADO2 = estado(izq, LstIzq, LstDer, N).

persona(alberto, 1).
persona(beatriz, 2).
persona(carlos, 5).
persona(dora, 10).
persona(emilio, 15).

personas_a_la_vez(3).
tiempo_cruzar(21).
listaPersonas(Nombres):-
    findall(Nombre, persona(Nombre,_),Nombres).

/*Selecciona de la lista las persona mas rapida en cruzarlo*/
personaRapida(R, [X|XS]):-
  min(X,Y, R),
  personaRapida(Y, XS).
personaRapida(P,[X]):-
  P = X.
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
  max(X,P1,P),
  selectMax(P1, XS).
selectMax(P,[X]):-
  P = X.

/*Selecciona las n personas mas lentas de la lista X de perosnas*/
% npersonas_mas_lentas(2,[alberto,beatriz,carlos,dora,emilio],[],R).
npersonas_mas_lentas(LISTA,RESTO,RESULTADO):-
  personas_a_la_vez(N),
  N1 is N - 1,
  npersonas_mas_lentas(N1,LISTA,[],RESULTADO),
  selectList(LISTA,RESULTADO,RESTO).

npersonas_lentas(N,LISTA,RESTO,RESULTADO):-
  npersonas_mas_lentas(N,LISTA,[],RESULTADO),
  selectList(LISTA,RESULTADO,RESTO).

npersonas_mas_lentas(0,_,A,A).

npersonas_mas_lentas(_,[],A,A).

npersonas_mas_lentas(N,LISTA,Acumulada,R):-
  N > 0,
  selectMax(Persona,LISTA),   %seleccionar la personas con max duracion
  select(Persona, LISTA, L),  %quitar esa persona de la lista, L es la nueva lista sin el elemento P
  insert(Persona, Acumulada, Acumulada1), %R1 es la lista R con el valor P insertado
  N1 is N - 1, 
  npersonas_mas_lentas(N1, L, Acumulada1, R).


selectPersonas(AMOVER, LISTA):-
  personas_a_la_vez(N),
  selectPersonas(N, LISTA, [], AMOVER).

selectPersonas(N, LISTA, Acumulada, Respuesta):-
  N > 0,
  N1 is N - 1,
  member(Persona,LISTA),
  select(Persona, LISTA, L),  %quitar esa persona de la lista, L es la nueva lista sin el elemento P
  insert(Persona, Acumulada, Acumulada1), %R1 es la lista R con el valor P insertado
  selectPersonas(N1, L, Acumulada1, Respuesta).

selectPersonas(0,_,R,R).
selectPersonas(_,[],R,R).
/*
* insert(ElementoInsertado, ListaVieja, ListaNueva)
*
* Inserta en orden una de las cosas en una lista.
* La relaci�n precedes/2 establece el orden de las cosas: z < g < m.
*/
insert(X,[Y|Ys],[X,Y|Ys]):-precedes(X,Y).   % Elemento va al inicio
insert(X,[Y|Ys],[Y|Zs]):-precedes(Y,X),insert(X,Ys,Zs).  % Insertar m�s adentro.
insert(X,[],[X]).                   % Insertar como �nico elemento.



% Caso determin�stico
precedes(X, Y):- 
  persona(X, _),
  persona(Y, _),
  X@<Y.

/*
* select(Elemento, ListaQueContieneElemento, ListaSinElemento)
*
* Extrae no determisticamente un elemento de una lista que lo contiene
* y obtiene la lista sin ese elemento.
*/
select(X,[X|Xs],Xs).                          % Extrae primer elemento.
select(X,[Y|Ys],[Y|Zs]):-select(X,Ys,Zs).     % Extrae elemento de m�s adentro.

selectList(LISTA,[X|XS],R):-    %LISTA DONDE SE VAN A EXTRAER
  select(X, LISTA, RESTO),     %X|XS ES LA LISTA DE ELEMENTOS A EXTRAER
  selectList(RESTO, XS, R).

selectList(R,[],R).

insertList([A|MOVER], LISTA, RESULTADO):-
  insert(A, LISTA, RESULTADO1),
  insertList(MOVER, RESULTADO1, RESULTADO).

insertList([], RESULTADO, RESULTADO).



