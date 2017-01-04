%--------------------------------------------------------------%
%   Beam Search                                                %
%   call:beam(+[[Start]],+Goal,+BeamSize,-Path,-ExploredNodes).%
%--------------------------------------------------------------%
beam([[Goal|Path]|_],Goal,_,[Goal|Path],0).
beam([Path|Queue],Goal,Beam,FinalPath,N) :-
    extend(Path,NewPaths), 
    append(Queue,NewPaths,Queue1), 
    sort_queue1(Queue1,Queue2),
    trim(Beam,Queue2,NewQueue),
    beam(NewQueue,Goal,Beam,FinalPath,M),
    N is M+1.

trim(N,[X|T],[X|V]) :- 
    N>0, !,
    M is N-1,
    trim(M,T,V).
trim(_,_,[]).