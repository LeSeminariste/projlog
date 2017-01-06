%--------------------------------------------------------------%
%   Uninformed Graph Searching Algorithms                      %
%   The graph should be represented as a set of facts          %
%   arc(Node1,Node2,Cost) loaded in the database               %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

	
%--------------------------------------------------------------%
%   Depth-first search by using a stack                        %
%   call: depth_first(+[[Start]],+Goal,-Path,-ExploredNodes).  %
%--------------------------------------------------------------%
depth_first([[Goal|Path]|_],Goal,[Goal|Path],0).
depth_first([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths), 
  %write('Path= '),write(Path),nl,
	%write('NewPaths = '), write(NewPaths), nl,
    append(NewPaths,Queue,NewQueue),
  %write('Queue= '),write(Queue),nl,
	%write('NewQueue = '), write(NewQueue), nl,
    depth_first(NewQueue,Goal,FinalPath,M),
    N is M+1.

extend([Node|Path],NewPaths) :-
    findall([NewNode,Node|Path],
            (arc(Node,NewNode,_), 
            \+ member(NewNode,Path)), % for avoiding loops
            NewPaths).


%--------------------------------------------------------------%
%   Breadth-first search                                       %
%   call: breadth_first(+[[Start]],+Goal,-Path,-ExploredNodes).%
%--------------------------------------------------------------%
breadth_first([[Goal|Path]|_],Goal,[Goal|Path],0).
breadth_first([Path|Queue],Goal,FinalPath,N) :-
    extend(Path,NewPaths), 
  %write('Path= '),write(Path),nl,
	%write('NewPaths = '), write(NewPaths), nl,
    append(Queue,NewPaths,NewQueue),
  %write('Queue= '),write(Queue),nl,
	%write('NewQueue = '), write(NewQueue), nl,
    breadth_first(NewQueue,Goal,FinalPath,M),
    N is M+1.

extend(NewPaths,[Node|Path]) :-
    findall([NewNode,Node|Path],
            (arc(Node,NewNode,_),
            \+ member(NewNode,Path)), % for avoiding loops
            NewPaths).

%--------------------------------------------------------------%
%   Calculating Path cost                                      %
%   call: path_cost(+Path,-Cost).                              %
%--------------------------------------------------------------%
path_cost([A,B],Cost) :-
    arc(A,B,Cost).
path_cost([A,B|T],Cost) :-
    arc(A,B,Cost1),
    path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

%--------------------------------------------------------------%
%   Solving problem                                            %
%   call: solve_d(+Start, +Goal, -SolPath, -ExploredNodes, -Cost)
%         solve_b(+Start, +Goal, -SolPath, -ExploredNodes, -Cost)
%--------------------------------------------------------------%
solve_b(Start,Goal,SolPath,ExploredNodes,Cost) :-
  breadth_first([[Start]],Goal,SolPath,ExploredNodes),
  reversel(SolPath,Reversedlist),
  path_cost(Reversedlist,Cost).

solve_d(Start,Goal,SolPath,ExploredNodes,Cost) :-
  depth_first([[Start]],Goal,SolPath,ExploredNodes),
  reversel(SolPath,Reversedlist),
  path_cost(Reversedlist,Cost).
  

%--------------------------------------------------------------%
%   Reversing list                                             %
%   call: reversel(+List_i, -List_F).                      %
%--------------------------------------------------------------%
reversel([],[]).
reversel([X|Xs],R) :-
  reversel(Xs,T),
  append(T,[X],R).                      

