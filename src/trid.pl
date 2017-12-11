/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

% tridplayer([A,12,B,C,9,D,E,F,6]).
% tridplayer([A,12,B,C,9,D,E,F,6,6,G,12,H,8,I,P]).

:- include('print.pl').
:- include('generictrid.pl').
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).


trid:- 
        write('***************************************************'),nl,
        write('********   Trid - PLOG - Version 1.0   ************'),nl,
        write('***************************************************'),nl,
        nl,nl,
        nl, write('1 - Play Game'),
        nl, write('2 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(Choice),
        menu(Choice).

menu(Choice):- Choice == 1, 
        genericChoose.
menu(Choice):- Choice == 2, 
        exit(_).

endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Trid Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.



/*
   L is the current list,
   P is the position where the element will be insert,
   E is the element to insert and
   R is the return, the new list
*/
replaceInThePosition(L, P, E, R) :-
        findall(X, (nth0(I,L,Y), (I == P -> X=E ; X=Y)), R).


randomvalues(R1,R2,R3,R4, TrianglesSize):- Final is TrianglesSize - 1,
        random(1,Final, R1),
        random(1,Final, R2),
        random(1,Final, R3),
        random(1,Final, R4),
        all_different([R1,R2,R3,R4]),
        labeling([],[R1,R2,R3,R4]).
