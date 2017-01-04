%--------------------------------------------------------------%
%   A map with town to town distances                          %
%   From:                                                      %
%   Russell, S and P. Norvig. Artificial Intelligence:         %
%   A modern approach, Prentice Hall, Upper Saddle River,      %
%   New Jersey, 1995                                           %
%--------------------------------------------------------------%

arc(arad,zerind,75).
arc(arad,sibiu,140).
arc(arad,timisoara,118).
arc(bucharest,fagaras,211).
arc(bucharest,pitesti,101).
arc(bucharest,giurgiu,90).
arc(bucharest,urziceni,85).
arc(craiova,dobreta,120).
arc(craiova,rimnicu,146).
arc(craiova,pitesti,138).
arc(dobreta,mehadia,75).
arc(dobreta,craiova,120).
arc(eforie,hirsova,86).
arc(fagaras,sibiu,99).
arc(fagaras,bucharest,211).
arc(giurgiu,bucharest,90).
arc(hirsova,urziceni,98).
arc(hirsova,eforie,86).
arc(iasi,neamt,87).
arc(iasi,vaslui,92).
arc(lugoj,timisoara,111).
arc(lugoj,mehadia,70).
arc(mehadia,lugoj,70).
arc(mehadia,dobreta,75).
arc(neamt,iasi,87).
arc(oradea,zerind,71).
arc(oradea,sibiu,151).
arc(pitesti,rimnicu,97).
arc(pitesti,craiova,138).
arc(pitesti,bucharest,101).
arc(rimnicu,sibiu,80).
arc(rimnicu,pitesti,97).
arc(rimnicu,craiova,146).
arc(sibiu,arad,140).
arc(sibiu,oradea,151).
arc(sibiu,fagaras,99).
arc(sibiu,rimnicu,80).
arc(timisoara,arad,118).
arc(timisoara,lugoj,111).
arc(urziceni,bucharest,85).
arc(urziceni,hirsova,98).
arc(urziceni,vaslui,142).
arc(vaslui,iasi,92).
arc(vaslui,urziceni,142).
arc(zerind,arad,75).
arc(zerind,oradea,71).

% heuristic function: straight line distance to bucharest
% (works only if the goal state is bucharest, see clause no2).

h(Node, Value) :- 
    straight_line_distance(Node, Value).

straight_line_distance(arad     ,366).
straight_line_distance(bucharest,  0).
straight_line_distance(craiova  ,160).
straight_line_distance(dobreta  ,242).
straight_line_distance(eforie   ,161).
straight_line_distance(fagaras  ,178).
straight_line_distance(giurgiu  , 77).
straight_line_distance(hirsova  ,151).
straight_line_distance(iasi     ,266).
straight_line_distance(lugoj    ,244).
straight_line_distance(mehadia  ,241).
straight_line_distance(neamt    ,234).
straight_line_distance(oradea   ,380).
straight_line_distance(pitesti  , 98).
straight_line_distance(rimnicu  ,193).
straight_line_distance(sibiu    ,253).
straight_line_distance(timisoara,329).
straight_line_distance(urziceni , 80).
straight_line_distance(vaslui   ,199).
straight_line_distance(zerind   ,374).
