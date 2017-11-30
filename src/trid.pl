/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

% tridplayer([0,12,0,0,9,0,0,0,6,6,0,12,0,8,0,0]).
% tridplayer([A,12,B,C,9,D,E,F,6,6,G,12,H,8,I,P]).

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
        nl, write('1 - Play Game choosing the variables'),
        nl, write('2 - Play Game in an automatic way'),
        nl, write('3 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(Choice),
        menu(Choice).

menu(Choice):- Choice == 1, 
        tridchoose(_). 
menu(Choice):- Choice == 2, 
        tridplayerauto(_).
menu(Choice):- Choice == 3, 
        exit(_).

tridchoose(_):- generateEmptyBoard(X),
        printBoard(X),
        write('write th inicial values of the variables with a number, using the list [a1,b1,b1,...] : '),nl,
        read(Choice),
        tridplayer(Choice).

tridplayer(Choice) :-
        variables(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Choice),
%        variablesat([A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1],Result),
        Value is 5,
        playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Value]),
        generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1),
        printBoard(X).

% retorna a lista da posi��o das variaveis na lista que recebe. devia ser os numeros mas pronto 
variablesat(List, Is) :-
        findall(N, nth1(N, List,  number(_)), Is).

variables(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Choice):- 
        nth1(1,Choice,A1),
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
        nth1(15,Choice,O1),
        nth1(16,Choice,P1).


playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Value]) :-
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
        I + J + F #= I1,
        G + K + L #= J1,
        G + H + L #= K1,
        L + H + M #= L1,
        I + H + M #= M1,       
        M + N + I #= N1,       
        I + J + N #= O1,       
        O + J + N #= P1,       
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]).

% preenche todo o tabuleiro de jogo com uma solu��o poss�vel
tridplayerauto(_) :-
        Value is 5,
        playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Value]),
        generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1),
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


endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Trid Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.
