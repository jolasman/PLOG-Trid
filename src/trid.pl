/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

% tridplayer([0,12,0,0,9,0,0,0,6,6,0,12,0,8,0]).
% tridplayer([A,12,B,C,9,D,E,F,6,6,G,12,H,8,I]).

:- include('print.pl').
:- include('board.pl').
?- use_module(library(system)).
?- use_module(library(lists)).
?- use_module(library(random)).
:- use_module(library(clpfd)).

trid:- 
        write('***************************************************'),nl,
        write('********   Trid - PLOG - Version 1.0   ************'),nl,
        write('***************************************************'),nl,
        nl,nl,
        nl, write('1 - Play'),
        nl, write('2 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(Choice),
        menu(Choice).

menu(Choice):- Choice == 1, 
        tridchoose(_). 
menu(Choice):- Choice == 2, 
        exit(_).

tridchoose(_):- generateEmptyBoard(X),
        printBoard(X),
        write('write th inicial values of the variables with a number, using the list [a1,b1,b1,...] : '),nl,
        read(Choice),
        tridplayer(Choice).

tridplayer(Choice) :-
        variables(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Choice),
        Value is 5,
        playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]),
        generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1),
        printBoard(X).

variables(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Choice):- nth1(1,Choice,A1),
        nth1(2,Choice,B1),
        nth1(3,Choice,C1),
        nth1(4,Choice,D1),
        nth1(5,Choice,E1),
        nth1(6,Choice,F1),
        nth1(7,Choice,G1),
        nth1(8,Choice,H1),
        nth1(9,Choice,I1),
        nth1(10,Choice,J1),
        nth1(11,Choice,K1),
        nth1(12,Choice,L1),
        nth1(13,Choice,M1),
        nth1(14,Choice,N1),
        nth1(15,Choice,O1).


playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
        all_different([A,B,D,G,K]),
        all_different([A,C,F,J,O]),
        all_different([K,L,M,N,O]),
        all_different([G,H,I,J]),
        all_different([D,E,F]),
        all_different([B,C]),
        all_different([B,E,I,N]),
        all_different([D,H,M]),
        all_different([G,L]),
        all_different([C,E,H,L]),
        all_different([F,I,M]),
        all_different([J,N]),
        A + B + C #= A1, 
        D + B + E #= B1,
        C + B + E #= C1,
        C + F + E #= D1,
        G + D + H #= E1,
        D + E + H #= F1,
        H + E + I #= G1,
        I + F + E #= H1,
        I + J + E #= I1,
        G + K + L #= J1,
        G + H + L #= K1,
        L + H + M #= L1,
        M + H + I #= M1,       
        M + N + I #= N1,       
        I + J + N #= O1,       
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]).

% preenche todo o tabuleiro de jogo com uma solução possível
tridplayerauto(_) :-
        Value is 5,
        playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]),
        generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1),
        printBoard(X).

playGameauto([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1],1,Value),
        all_different([A,B,D,G,K]),
        all_different([A,C,F,J,O]),
        all_different([K,L,M,N,O]),
        all_different([G,H,I,J]),
        all_different([D,E,F]),
        all_different([B,C]),
        all_different([B,E,I,N]),
        all_different([D,H,M]),
        all_different([G,L]),
        all_different([C,E,H,L]),
        all_different([F,I,M]),
        all_different([J,N]),
        A + B + C #= A1, 
        D + B + E #= B1,
        C + B + E #= C1,
        C + F + E #= D1,
        G + D + H #= E1,
        D + E + H #= F1,
        H + E + I #= G1,
        I + F + E #= H1,
        I + J + E #= I1,
        G + K + L #= J1,
        G + H + L #= K1,
        L + H + M #= L1,
        M + H + I #= M1,       
        M + N + I #= N1,       
        I + J + N #= O1,       
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1]).

%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        A1 \= 0,
%        A + B + C #= A1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        B1 \= 0,
%        D + E + B #= B1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        C1 \= 0,
%        B + C + E #= C1,       
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        D1 \= 0,
%        E + F + C #= D1,       
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        E1 \= 0,
%        G + H + D #= E1,
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        F1 \= 0,
%        D + E + H #= F1,       
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        G1 \= 0,
%        H + I + E #= G1,   
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        H1 \= 0,
%        E + F + I #= H1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        I1 \= 0,
%        I + J + F #= I1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        J1 \= 0,
%        G + K + L #= J1,
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        K1 \= 0,
%        G + H + L #= K1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        L1 \= 0,
%        L + H + M #= L1,        
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        M1 \= 0,
%        H + I + M #= M1,       
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        N1 \= 0,
%        M + N + I #= N1,       
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
%
%playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,Value]) :-
%        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
%        all_different([A,B,D,G,K]),
%        all_different([A,C,F,J,O]),
%        all_different([K,L,M,N,O]),
%        all_different([G,H,I,J]),
%        all_different([D,E,F]),
%        all_different([B,C]),
%        all_different([B,E,I,N]),
%        all_different([D,H,M]),
%        all_different([G,L]),
%        all_different([C,E,H,L]),
%        all_different([F,I,M]),
%        all_different([J,N]),
%        O1 \= 0,
%        I + J + N #= O1,,        %
%        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).




endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Trid Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.
