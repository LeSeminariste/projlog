%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% **************************************************************
% *    RIVER TEST (originally game from BlackBerry devices)    *
% **************************************************************
%
%  Un etat est un simple 7-tuplet [F,S,M,D,P,T,R].
%  F : father
%  S : sons
%  M : mother
%  D : daughters
%  P : policeman
%  T : thief
%  R : raft
%
%  Dans cette correction, les predicats ont le meme nom
%  que dans l'enonce.
%  Pour voir les solutions calculees sans les pointilles,
%  faire : resultat(_,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mode debug (1 = VRAI; 0 = FAUX)
debug(0).
%debug(1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% etat initial et final
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% etat_init, etat_final
etat_init([1,2,1,2,1,1,0]).
etat_final([0,0,0,0,0,0,1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% les predicats suivants permettent l'extraction d une
% variable de la liste Etat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   father([F,_,_,_,_,_,_],F).
     sons([_,S,_,_,_,_,_],S).
   mother([_,_,M,_,_,_,_],M).
daughters([_,_,_,D,_,_,_],D).
policeman([_,_,_,_,P,_,_],P).
    thief([_,_,_,_,_,T,_],T).
     raft([_,_,_,_,_,_,R],R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% legal_vars([F,S,M,D,P,T,R])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% variables toujours dans leur valeurs possibles apres un deplacement
legal_vars([F,S,M,D,P,T,R]) :-
	F =< 1, F >= 0,
	S =< 2, S >= 0,
	M =< 1, M >= 0,
	D =< 2, D >= 0,
	T =< 1, T >= 0,
	P =< 1, P >= 0,
	R =< 1, R >= 0.


% c'est bien un driver autorise qui a conduit le raft
legal_raft_driver(E1,E2):- drive_father(E1,E2).
legal_raft_driver(E1,E2):- drive_mother(E1,E2).
legal_raft_driver(E1,E2):- drive_policeman(E1,E2).

drive_father(E1,E2) :- father(E1,F1), father(E2,F2), cross(F1,F2).
drive_mother(E1,E2) :- mother(E1,M1), mother(E2,M2), cross(M1,M2).
drive_policeman(E1,E2) :- policeman(E1,P1), policeman(E2,P2), cross(P1,P2).

cross(V1,V2) :- V2 =:= V1+1.
cross(V1,V2) :- V2 =:= V1-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Controle du voleur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tnD(Etat)
%% voleur non dangereux
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tnD(Etat):-tuC(Etat) ; tAlone(Etat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tuC(Etat) 
%% thief under Control (tuC)
%% le policier est avec le voleur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tuC([_,_,_,_,V,V,_]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tAlone(Etat)
%% thiefAlone 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tAlone(Etat):- tAlone1(Etat).
tAlone(Etat):- tAlone2(Etat).

tAlone1([0,0,0,0,0,1,_]). % le voleur est seul sur Rive1
tAlone2([1,2,1,2,1,0,_]). % le voleur est seul sur Rive2



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% safe(Etat)
% les personnages sont saufs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tout le monde est safe
safe(Etat):-	safe_D(Etat),
				safe_S(Etat),
				safe_Family(Etat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% safe_D(Etat)
% les filles sont sauves vis-a-vis du pere
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rive de départ pour la mere
safe_D([V,_,V,_,_,_,_]). %% Mere et pere du meme cote : filles en securite
safe_D([_,_,1,2,_,_,_]). %% Mere et 2 filles ensembles
%safe_D([1,_,1,_,_,_,_]). %% Mere et pere du meme cote
%safe_D([1,_,1,1,_,_,_]). %% Une fille seule : Mere et pere du meme coté
% rive d arrivee pour la mere
%safe_D([0,_,0,1,_,_,_]). %% Une fille seule : Mere et pere du meme coté
%safe_D([0,_,0,_,_,_,_]). %% Mere et pere du meme cote
safe_D([_,_,0,0,_,_,_]). %% Mere et 2 filles ensembles
%safe_D(E):-(debug(1),write('safe_D fail      :'),writeln(E),fail);fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% safe_S(Etat)
% les garcons sont saufs vis-a-vis de la mere
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
safe_S([V,_,V,_,_,_,_]). %% Mere et pere du meme cote : garcons en securite
% rive de départ pour le pere
safe_S([1,2,_,_,_,_,_]). %% Pere et 2 garcons ensembles
%safe_S([1,_,1,_,_,_,_]). %% Pere et mere du meme cote
%safe_S([1,1,1,_,_,_,_]). %% Un garcon seul : Mere et pere du meme coté
% rive d arrivee pour le pere
%safe_S([0,1,0,_,_,_,_]). %% Un garcon seul : Mere et pere du meme coté
%safe_S([0,_,0,_,_,_,_]). %% Mere et pere du meme cote
safe_S([0,0,_,_,_,_,_]). %% Pere et 2 garcons ensembles
%safe_S([F,S,M,D,P,T,R]):-F=:=1,S=:=2,M=:=1,D=:=2,P=:=1,T=:=1,R=:=0.
%safe_S([1,2,1,2,1,1,0]).
%safe_S(E):-(debug(1),write('safe_S fail      :'),writeln(E),fail);fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% la famille est safe
safe_Family(Etat):-tnD(Etat).
%safe_Family(Etat):-(debug(1),write('safe_Family fail :'), writeln(Etat),fail);fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% legal(Etat1,Etat2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
legal(Etat1,Etat2) :- 	
	deplacement_legal(Etat1,Etat2),
	safe(Etat2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deplacement_legal(E1,E2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deplacement_legal(E1,E2) :-
	legal_vars(E2),
	legal_raft_driver(E1,E2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transition_possible
% -> genere les états successeurs possibles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transition_possible(E1,E2):-
	deplacement_par_radeau(E1,E2),
	legal(E1,E2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deplacement_par_radeau(Etat,Etat2)
% -> genere un etat successeur
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deplacement_par_radeau(Etat,Etat2):-
	radeau(Etat,Etat2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% radeau(Etat1,Etat2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------------------------
% Depart vers Arrivee
%%--------------------------------------------

% traversees de la mere
radeau([F,S,M,D,P,T,0],[F,S,M2,D,P,T,1]):-      M2 is M - 1.              /* 1M    traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M2,D,P,T,1]):-	    M2 is M - 1, F2 is F - 1. /* 1M+1F traverse */
radeau([F,S,M,D,P,T,0],[F,S2,M2,D,P,T,1]):-     M2 is M - 1, S2 is S - 1. /* 1M+1S traverse */
radeau([F,S,M,D,P,T,0],[F,S,M2,D2,P,T,1]):-     M2 is M - 1, D2 is D - 1. /* 1M+1D traverse */
radeau([F,S,M,D,P,T,0],[F,S,M2,D,P2,T,1]):-     M2 is M - 1, P2 is P - 1. /* 1M+1P traverse */
radeau([F,S,M,D,P,T,0],[F,S,M2,D,P,T2,1]):-     M2 is M - 1, T2 is T - 1. /* 1M+1T traverse */

% traversees du pere
radeau([F,S,M,D,P,T,0],[F2,S,M,D,P,T,1]):-      F2 is F - 1.              /* 1F    traverse */
radeau([F,S,M,D,P,T,0],[F2,S2,M,D,P,T,1]):-     F2 is F - 1, S2 is S - 1. /* 1F+1S traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M2,D,P,T,1]):-     F2 is F - 1, M2 is M - 1. /* 1F+1M traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M,D2,P,T,1]):-     F2 is F - 1, D2 is D - 1. /* 1F+1D traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M,D,P2,T,1]):-     F2 is F - 1, P2 is P - 1. /* 1F+1P traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M,D,P,T2,1]):-     F2 is F - 1, T2 is T - 1. /* 1F+1T traverse */

% traversees du policier
radeau([F,S,M,D,P,T,0],[F,S,M,D,P2,T,1]):-      P2 is P - 1.              /* 1P    traverse */
radeau([F,S,M,D,P,T,0],[F2,S,M,D,P2,T,1]):-		P2 is P - 1, F2 is F - 1. /* 1P+1F traverse */
radeau([F,S,M,D,P,T,0],[F,S2,M,D,P2,T,1]):-     P2 is P - 1, S2 is S - 1. /* 1P+1S traverse */
radeau([F,S,M,D,P,T,0],[F,S,M2,D,P2,T,1]):-		P2 is P - 1, M2 is M - 1. /* 1P+1M traverse */
radeau([F,S,M,D,P,T,0],[F,S,M,D2,P2,T,1]):-     P2 is P - 1, D2 is D - 1. /* 1P+1D traverse */
radeau([F,S,M,D,P,T,0],[F,S,M,D,P2,T2,1]):-     P2 is P - 1, T2 is T - 1. /* 1P+1T traverse */

%%--------------------------------------------
%% de la rive d arrivee vers celle de depart
%%--------------------------------------------
% traversees de la mere
radeau([F,S,M,D,P,T,1],[F,S,M2,D,P,T,0]):-      M2 is M + 1.              /* 1M    traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M2,D,P,T,0]):-		M2 is M + 1, F2 is F + 1. /* 1M+1F traverse */
radeau([F,S,M,D,P,T,1],[F,S2,M2,D,P,T,0]):-     M2 is M + 1, S2 is S + 1. /* 1M+1S traverse */
radeau([F,S,M,D,P,T,1],[F,S,M2,D2,P,T,0]):-     M2 is M + 1, D2 is D + 1. /* 1M+1D traverse */
radeau([F,S,M,D,P,T,1],[F,S,M2,D,P2,T,0]):-     M2 is M + 1, P2 is P + 1. /* 1M+1P traverse */
radeau([F,S,M,D,P,T,1],[F,S,M2,D,P,T2,0]):-     M2 is M + 1, T2 is T + 1. /* 1M+1T traverse */

% traversees du pere
radeau([F,S,M,D,P,T,1],[F2,S,M,D,P,T,0]):-      F2 is F + 1.              /* 1F    traverse */
radeau([F,S,M,D,P,T,1],[F2,S2,M,D,P,T,0]):-     F2 is F + 1, S2 is S + 1. /* 1F+1S traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M2,D,P,T,0]):-     F2 is F + 1, M2 is M + 1. /* 1F+1M traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M,D2,P,T,0]):-     F2 is F + 1, D2 is D + 1. /* 1F+1D traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M,D,P2,T,0]):-     F2 is F + 1, P2 is P + 1. /* 1F+1P traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M,D,P,T2,0]):-     F2 is F + 1, T2 is T + 1. /* 1F+1T traverse */

% traversees du policier
radeau([F,S,M,D,P,T,1],[F,S,M,D,P2,T,0]):-      P2 is P + 1.              /* 1P    traverse */
radeau([F,S,M,D,P,T,1],[F2,S,M,D,P2,T,0]):-		P2 is P + 1, F2 is F + 1. /* 1P+1F traverse */
radeau([F,S,M,D,P,T,1],[F,S2,M,D,P2,T,0]):-     P2 is P + 1, S2 is S + 1. /* 1P+1S traverse */
radeau([F,S,M,D,P,T,1],[F,S,M2,D,P2,T,0]):-		P2 is P + 1, M2 is M + 1. /* 1P+1M traverse */
radeau([F,S,M,D,P,T,1],[F,S,M,D2,P2,T,0]):-     P2 is P + 1, D2 is D + 1. /* 1P+1D traverse */
radeau([F,S,M,D,P,T,1],[F,S,M,D,P2,T2,0]):-     P2 is P + 1, T2 is T + 1. /* 1P+1T traverse */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Affichage de la solution sous forme textuelle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------
% dans quel sens est le déplacement ('va vers' ou 'revient de')
%-------------------------------------------
direction(E1, E2, Direction):- raft(E1,R1),raft(E2,R2), Direction is R1 - R2.

%-------------------------------------------
% qui a traverse ?
%-------------------------------------------
cross_father(E1,E2) :- father(E1,V1), father(E2,V2), cross(V1,V2).
cross_mother(E1,E2) :- mother(E1,V1), mother(E2,V2), cross(V1,V2).
cross_sons(E1,E2) :- sons(E1,V1), sons(E2,V2), cross(V1,V2).
cross_daughters(E1,E2) :- daughters(E1,V1), daughters(E2,V2), cross(V1,V2).
cross_policeman(E1,E2) :- policeman(E1,V1), policeman(E2,V2), cross(V1,V2).
cross_thief(E1,E2) :- thief(E1,V1), thief(E2,V2), cross(V1,V2).

%-------------------------------------------
% on affiche la personne qui a traverse
% et si elle n'est pas seule, on affiche
% aussi la deuxieme : one_more_cross
% l'etat E1bis, est l'état E1 prive de la
% personne qui a traverse pour ensuite voir 
% si il y a une autre différence entre E1bis
% et E2, c-a-d un 2nd deplacement.
%-------------------------------------------
one_cross(E1,E2,E1bis):-cross_father(E1,E2),write('Le pere '),sub_father(E1,E2,E1bis),one_more_cross(E1bis,E2),!.
one_cross(E1,E2,E1bis):-cross_mother(E1,E2),write('La mere '),sub_mother(E1,E2,E1bis),one_more_cross(E1bis,E2),!.
one_cross(E1,E2,E1bis):-cross_policeman(E1,E2),write('Le policier '),sub_policeman(E1,E2,E1bis),one_more_cross(E1bis,E2),!.
one_cross(E1,E2,E1bis):-cross_sons(E1,E2),write('Un garcon '),sub_sons(E1,E2,E1bis),one_more_cross(E1bis,E2),!.

one_cross(E1,E2,E1bis):-cross_daughters(E1,E2),write('Une fille '),sub_daughters(E1,E2,E1bis), one_more_cross(E1bis,E2),!.

one_cross(E1,E2,E1bis):-cross_thief(E1,E2),write('Le voleur '),sub_thief(E1,E2,E1bis),one_more_cross(E1bis,E2),!.

one_cross(E1,E2,_):-no_one_cross_more(E1,E2).

one_more_cross(E1,E2):-cross_father(E1,E2),   direction(E1,E2,1),     write('et le pere reviennent de la rive d arrivee vers la rive de depart'),!.
one_more_cross(E1,E2):-cross_sons(E1,E2),     direction(E1,E2,1),   write('et un garcon reviennent de la rive d arrivee vers la rive de depart'),!.
one_more_cross(E1,E2):-cross_mother(E1,E2),   direction(E1,E2,1),     write('et la mere reviennent de la rive d arrivee vers la rive de depart'),!.
one_more_cross(E1,E2):-cross_daughters(E1,E2),direction(E1,E2,1),   write('et une fille reviennent de la rive d arrivee vers la rive de depart'),!.
one_more_cross(E1,E2):-cross_policeman(E1,E2),direction(E1,E2,1), write('et le policier reviennent de la rive d arrivee vers la rive de depart'),!.
one_more_cross(E1,E2):-cross_thief(E1,E2),    direction(E1,E2,1),   write('et le voleur reviennent de la rive d arrivee vers la rive de depart'),!.

one_more_cross(E1,E2):-cross_father(E1,E2),   direction(E1,E2,-1),    write('et le pere traversent de la rive de depart vers la rive d arrivee'),!.
one_more_cross(E1,E2):-cross_sons(E1,E2),     direction(E1,E2,-1),  write('et un garcon traversent de la rive de depart vers la rive d arrivee'),!.
one_more_cross(E1,E2):-cross_mother(E1,E2),   direction(E1,E2,-1),    write('et la mere traversent de la rive de depart vers la rive d arrivee'),!.
one_more_cross(E1,E2):-cross_daughters(E1,E2),direction(E1,E2,-1),  write('et une fille traversent de la rive de depart vers la rive d arrivee'),!.
one_more_cross(E1,E2):-cross_policeman(E1,E2),direction(E1,E2,-1),write('et le policier traversent de la rive de depart vers la rive d arrivee'),!.
one_more_cross(E1,E2):-cross_thief(E1,E2),    direction(E1,E2,-1),  write('et le voleur traversent de la rive de depart vers la rive d arrivee'),!.

one_more_cross(E1,E2):-direction(E1,E2,-1),write('traverse de la rive de depart vers la rive d arrivee'),!,true.
one_more_cross(E1,E2):-direction(E1,E2, 1),write('revient de la rive d arrivee vers la rive de départ'),!,true.

no_one_cross_more(E,E).

father_pos(0).
sons_pos(1).
mother_pos(2).
daughters_pos(3).
policeman_pos(4).
thief_pos(5).

sub_father(E1,E2,E3)   :-direction(E1,E2,Dir),   father_pos(Pos),sub(Pos,Dir,E1,E3).
sub_mother(E1,E2,E3)   :-direction(E1,E2,Dir),   mother_pos(Pos),sub(Pos,Dir,E1,E3).
sub_sons(E1,E2,E3)     :-direction(E1,E2,Dir),     sons_pos(Pos),sub(Pos,Dir,E1,E3).
sub_daughters(E1,E2,E3):-direction(E1,E2,Dir),daughters_pos(Pos),sub(Pos,Dir,E1,E3).
sub_policeman(E1,E2,E3):-direction(E1,E2,Dir),policeman_pos(Pos),sub(Pos,Dir,E1,E3).
sub_thief(E1,E2,E3)    :-direction(E1,E2,Dir),    thief_pos(Pos),sub(Pos,Dir,E1,E3).

sub(Pos,Dir,E2,E3):-
    nth0(Pos, E2, Elem1), %Binds Nth to Elem1
    Elem2 is Elem1 - Dir,
    replace2(E2, Pos, Elem2, E3).

print_sol(E1, E2):- one_cross(E1,E2,_).

nb_elt([],0).
nb_elt([_|L],M):-nb_elt(L,N),M is N+1.

replace2(L, I, X, R) :-
    Dummy =.. [dummy|L],
    J is I + 1,
    setarg(J, Dummy, X),
    Dummy =.. [dummy|R].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calcul et affichage de la solution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solution
solution(Etat,Chemin,Solution):-etat_final(Etat),!,Solution=Chemin.
solution(Etat,Chemin,Solution):-transition_possible(Etat,EtatSuiv),not(member(EtatSuiv,Chemin)),solution(EtatSuiv,[EtatSuiv|Chemin],Solution).
% avec profondeur limite
solution(Etat,Chemin,Solution,Proflimite):-Proflimite>0,etat_final(Etat),!,Solution=Chemin.
solution(Etat,Chemin,Solution,Proflimite):-Proflimite>0,transition_possible(Etat,EtatSuiv),not(member(EtatSuiv,Chemin)),solution(EtatSuiv,[EtatSuiv|Chemin],Solution,Proflimite-1).

% resoudre
resoudre(S,N):-etat_init(E),!,writeln('Searching...'),solution(E,[E],S),nb_elt(S,N).
% avec profondeur limite
resoudre(S,N,Proflimite):-etat_init(E),!,writeln('Searching...'),solution(E,[E],S,Proflimite),nb_elt(S,N).


% pour afficher les resultats un par un au lieu d une liste avec des pointilles


tete(X,[X|Xs],Xs).
affich2([],_).
affich2([_],_).
affich2([X|L],N):-  tete(Y,L,L1),
    write('Etat '),
    write(N),
    write( : ),
    write(X),
    write('Etat '), B is N+1,
    write(B),
    write( : ),
    write(Y),
    write('\n'),
    print_sol(X,Y),
    write('\n'),
    M is N+1,
    affich2([Y|L1],M).


%affichage final 
resultat(SR,NB):-resoudre(SR,M),reverse(SR,S), NB is M-1, write('Solution en '), write(NB), write(' coups.\n'), affich2(S,1).
% avec profondeur limite
resultat(SR,NB,Proflimite):-resoudre(SR,M,Proflimite),reverse(SR,S), NB is M-1, write('Solution en '), write(NB), write(' coups.\n'), affich2(S,1).
