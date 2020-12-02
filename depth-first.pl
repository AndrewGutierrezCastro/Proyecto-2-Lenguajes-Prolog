/*
 * solve_dfs(State,History,Moves)
 *   Moves es la secuencia de movidas requeridas para
 *   alcanzar un estado final deseado a partir de State.
 *   History contiene los estados previamente visitados.
 */
 
% Si el Estado actual es un estado final, no hay que moverse.
solve_dfs(Estado,_,[]) :- final_state(Estado).

/*
 * Si el Estado actual no es un estado final, genera una movida
 * para desplazarse a un nuevo estado, y continua la b�squeda a
 * partir de ese nuevo estado.
 */
solve_dfs(Estado,Historia,[Movida|Movidas]) :-
      move(Estado,Movida),               % generar una nueva Movida
      update(Estado,Movida,Estado2),     % calcula nuevo estado usando Movida
      legal(Estado2),                    % nuevo estado debe ser legal
      not(member(Estado2,Historia)),     % debe ser primera vez que se llega al nuevo estado
      solve_dfs(Estado2,[Estado2|Historia],Movidas).   % continuar a partir de nuevo estado

/*
 * Inicializa un problema y lo resuelve.
 *   Problema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_dfs(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_dfs(Estado,[Estado],Movidas).  % inicia resoluci�n desde Estado

initial_state(puente,estado(izq,[alberto,beatriz,carlos,dora,emilio],[],N)):-
      tiempo_cruzar(N).

final_state(estado(der,[],[alberto,beatriz,carlos,dora,emilio],_)).

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

move(estado(izq,LstIzq,LstDer,_),AMOVER):-
      findall(MOVIDAS, selectPersonas(MOVIDAS, LstIzq), SOLREPS),
      sort(SOLREPS,SOLsinREP),
      member(AMOVER, SOLsinREP).
      
/*
move(estado(izq,LstIzq,LstDer,_), AMOVER):-     %Devolver en AMOVER la lista de 
      Si se quiere mover de la izquierda a la derecha
      se revisa si a la izquierda ya hay una persona rapida
      para pasar a npersonas lentas y asi reducir el tiempo total
      LstDer \= [],                                           %debe haber al menos una persona en la lista derecha
      personaRapida(PersonaIzq, LstIzq),
      personaRapida(PersonaDer, LstDer),
      persona(PersonaIzq, PerIzqNum),
      persona(PersonaDer, PerDerNum),
      PerDerNum < PerIzqNum,
      personas_a_la_vez(N),              
      npersonas_lentas(N ,LstIzq, RESTO, AMOVER). */    %elementos a mover de la lista lstIzq
                                                      %elementos a mover de la lista lstIzq                                   %elementos a mover de la lista LstDer
  
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