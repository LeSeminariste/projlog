%--------------------------------------------------------------%
%   Informed Graph Searching Algorithms                        %
%   The graph should be represented as a set of facts          %
%   arc(Node1,Node2,Cost) loaded in the database               %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

%--------------------------------------------------------------%
%   Best-First Search                                          %
%   call: best_first(+[[Start]],+Goal,-Path,-ExploredNodes).   %
%--------------------------------------------------------------%
best_first([[Goal|Path]|_],Goal,[Goal|Path],0).
best_first([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths), 
    append(Queue,NewPaths,Queue1),
    sort_queue1(Queue1,NewQueue), wrq(NewQueue),
    best_first(NewQueue,Goal,FinalPath,M),
    N is M+1.

extend([Node|Path],NewPaths) :-
    findall([NewNode,Node|Path],
            (arc(Node,NewNode,_), 
            \+ member(NewNode,Path)), % for avoiding loops
            NewPaths).

sort_queue1(L,L2) :-
    swap1(L,L1), !,
    sort_queue1(L1,L2).
sort_queue1(L,L).

swap1([[A1|B1],[A2|B2]|T],[[A2|B2],[A1|B1]|T]) :-
    hh(A1,W1),
    hh(A2,W2),
    W1>W2.
swap1([X|T],[X|V]) :-
    swap1(T,V).

%--------------------------------------------------------------%
%   A*                                                         %
%   call: a_star(+[[Start]],+Goal,-Path,-ExploredNodes).       %
%--------------------------------------------------------------%
a_star([[Goal|Path]|_],Goal,[Goal|Path],0).
a_star([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths),
    append(Queue,NewPaths,Queue1), 
    sort_queue2(Queue1,NewQueue), wrq(NewQueue),
    a_star(NewQueue,Goal,FinalPath,M),
    N is M+1.

sort_queue2(L,L2) :-
    swap2(L,L1), !,
    sort_queue2(L1,L2).
sort_queue2(L,L).

swap2([X,Y|T],[Y,X|T]) :-
    f(X,DX),
    f(Y,DY),
    DX>DY.
swap2([X|T],[X|V]) :-
    swap2(T,V).

%--------------------------------------------------------------%
%   Heuristic functions                                        %
%--------------------------------------------------------------%

% check if the heuristic function is ok.
hh(State, Value) :- 
    h(State,Value),
    number(Value), !.
hh(State, Value) :- 
   write('Incorrect heuristic functionh: '),
   write(h(State, Value)), nl,
   abort.

%h(_,1).  % default value (must be redefined for each problem)   % Déja présent dans map.pl                    

f([X|T],F) :-                   % for A* search
    reverse_path_cost([X|T],G), % calculate G
    hh(X,H),                    % calculate H
    F is G+H.                   % F = G + H

reverse_path_cost([A,B],Cost) :-
    arc(B,A,Cost).
reverse_path_cost([A,B|T],Cost) :-
    arc(B,A,Cost1),
    reverse_path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

%--------------------------------------------------------------%


wrq(Q) :- length(Q,N), write(N), write(' ').

solve_best(Start,Goal,SolPath,ExploredNodes,Cost) :-
  best_first([[Start]],Goal,SolPath,ExploredNodes),
  reverse_path_cost(SolPath,Cost).

solve_a(Start,Goal,SolPath,ExploredNodes,Cost) :-
  a_star([[Start]],Goal,SolPath,ExploredNodes),
  reverse_path_cost(SolPath,Cost).
