/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

:- include('print.pl').
:- include('board.pl').


?- use_module(library(system)).
?- use_module(library(random)).
:- use_module(library(clpfd)).

trid(_):- 
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
        tridplayer(_). 
menu(Choice):- Choice == 2, 
        exit(_).

tridplayer(X) :-
%        printMenu(X),
        Value is 5,
        playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]),
        generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O),
        printBoard(X).

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.

%se o player1 ganhar faz endgame, se nao ganhar verifica se o player dois ganha ou nao.
%se o player2 ganhar acaba, se nao ganhar chama de novo
playGame([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]) :- domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],1,Value),
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
        G + K + L #= 6,
        L + H + M #= 12,
        M + N + I #= 8,
        G + H + D #= 9,
        I + J + F #= 6,
        D + E + B #= 12,        
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Value]).
        
        
        
        
        

generateRandomMove(Xpos,Ypos) :-
        random(1, 11, Ypos),
        randomX(Ypos,Xpos).

randomX(Ypos,Xpos) :- (Ypos == 1,random(1, 5, Xpos));Ypos == 11,random(1, 5, Xpos).
randomX(Ypos,Xpos) :- (Ypos == 2,random(1, 6, Xpos));Ypos == 10,random(1, 6, Xpos).
randomX(Ypos,Xpos) :- (Ypos == 3,random(1, 7, Xpos));Ypos == 9,random(1, 7, Xpos).
randomX(Ypos,Xpos) :- (Ypos == 4,random(1, 8, Xpos));Ypos == 8,random(1, 8, Xpos).
randomX(Ypos,Xpos) :- (Ypos == 5,random(1, 9, Xpos));Ypos == 7,random(1, 9, Xpos).
randomX(Ypos,Xpos) :- Ypos == 6,random(1, 10, Xpos).


endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Boku Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.


