%--------------------------------------------------------------%
%   8-puzzle problem domain                                    %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%
%   0 1 2
%   3 4 5   =>  board(0,1,2,3,4,5,6,7,8)
%   6 7 8
%--------------------------------------------------------------%

% empty tile placed on position 1
arc(board(0,B,C,D,E,F,G,H,I),board(B,0,C,D,E,F,G,H,I),2).
arc(board(0,B,C,D,E,F,G,H,I),board(D,B,C,0,E,F,G,H,I),2).

% empty tile placed on position 2
arc(board(A,0,C,D,E,F,G,H,I),board(0,A,C,D,E,F,G,H,I),2).
arc(board(A,0,C,D,E,F,G,H,I),board(A,C,0,D,E,F,G,H,I),2).
arc(board(A,0,C,D,E,F,G,H,I),board(A,E,C,D,0,F,G,H,I),2).

% empty tile placed on position 3
arc(board(A,B,0,D,E,F,G,H,I),board(A,0,B,D,E,F,G,H,I),2).
arc(board(A,B,0,D,E,F,G,H,I),board(A,B,F,D,E,0,G,H,I),2).

% empty tile placed on position 4
arc(board(A,B,C,0,E,F,G,H,I),board(0,B,C,A,E,F,G,H,I),2).
arc(board(A,B,C,0,E,F,G,H,I),board(A,B,C,E,0,F,G,H,I),2).
arc(board(A,B,C,0,E,F,G,H,I),board(A,B,C,G,E,F,0,H,I),2).

% empty tile placed on position 5
arc(board(A,B,C,D,0,F,G,H,I),board(A,0,C,D,B,F,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,0,D,F,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,D,F,0,G,H,I),2).
arc(board(A,B,C,D,0,F,G,H,I),board(A,B,C,D,H,F,G,0,I),2).

% empty tile placed on position 6
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,0,D,E,C,G,H,I),2).
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,C,D,0,E,G,H,I),2).
arc(board(A,B,C,D,E,0,G,H,I),board(A,B,C,D,E,I,G,H,0),2).

% empty tile placed on position 7
arc(board(A,B,C,D,E,F,0,H,I),board(A,B,C,0,E,F,D,H,I),2).
arc(board(A,B,C,D,E,F,0,H,I),board(A,B,C,D,E,F,H,0,I),2).

% empty tile placed on position 8
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,0,F,G,E,I),2).
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,E,F,0,G,I),2).
arc(board(A,B,C,D,E,F,G,0,I),board(A,B,C,D,E,F,G,I,0),2).

% empty tile placed on position 9
arc(board(A,B,C,D,E,F,G,H,0),board(A,B,C,D,E,0,G,H,F),2).
arc(board(A,B,C,D,E,F,G,H,0),board(A,B,C,D,E,F,G,0,H),2).


% Heuristic (number of misplaced tiles)
% Note that the goal state is (0,1,2,3,4,5,6,7,8)
% For another goal Change the defnition below

h(board(A,B,C,D,E,F,G,H,I),W) :-
    distance_manhattan(board(A,B,C,D,E,F,G,H,I),W).

distance([],[],0).
distance([X|T],[X|V],W) :- !,
    distance(T,V,W).
distance([_|T],[_|V],W) :-
    distance(T,V,W1),
    W is W1+1.

distance_manhattan(board(A,B,C,D,E,F,G,H,I),W) :-
	d0(A,W0),
	d1(B,W1),
	d2(C,W2),
	d3(D,W3),
	d4(E,W4),
	d5(F,W5),
	d6(G,W6),
	d7(H,W7),
	d8(I,W8),
	W is W0 + W1 + W2 + W3 + W4 + W5 + W6 + W7 + W8.

d0(0,0).
d0(1,1).
d0(2,2).
d0(3,1).
d0(4,2).
d0(5,3).
d0(6,2).
d0(7,3).
d0(8,4).

d1(0,0).
d1(1,0).
d1(2,1).
d1(3,2).
d1(4,1).
d1(5,2).
d1(6,3).
d1(7,2).
d1(8,3).

d2(0,0).
d2(1,1).
d2(2,0).
d2(3,3).
d2(4,2).
d2(5,1).
d2(6,4).
d2(7,3).
d2(8,2).

d3(0,0).
d3(1,2).
d3(2,3).
d3(3,0).
d3(4,1).
d3(5,2).
d3(6,1).
d3(7,2).
d3(8,3).

d4(0,0).
d4(1,1).
d4(2,2).
d4(3,1).
d4(4,0).
d4(5,1).
d4(6,2).
d4(7,1).
d4(8,2).

d5(0,0).
d5(1,2).
d5(2,1).
d5(3,2).
d5(4,1).
d5(5,0).
d5(6,3).
d5(7,2).
d5(8,1).

d6(0,0).
d6(1,3).
d6(2,4).
d6(3,1).
d6(4,2).
d6(5,3).
d6(6,0).
d6(7,1).
d6(8,2).

d7(0,0).
d7(1,2).
d7(2,3).
d7(3,2).
d7(4,1).
d7(5,2).
d7(6,1).
d7(7,0).
d7(8,1).

d8(0,0).
d8(1,3).
d8(2,2).
d8(3,3).
d8(4,2).
d8(5,1).
d8(6,2).
d8(7,1).
d8(8,0).
