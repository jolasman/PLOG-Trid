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
        generateEmptyBoard(X),
        printBoard(X).
%        playGame(X).

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.

%se o player1 ganhar faz endgame, se nao ganhar verifica se o player dois ganha ou nao.
%se o player2 ganhar acaba, se nao ganhar chama de novo
playGame(X) :- p1(P1xpos, P1ypos),          
        playerturn(X, X1, P1xpos, P1ypos, 'W'),
        printBoard(X1),
        (isWinCondition(X1,P1xpos,P1ypos),nl, endGame(_);
         p2(P2xpos, P2ypos),          
         playerturn(X1, X2, P2xpos,P2ypos, 'B'),
         printBoard(X2),
         (isWinCondition(X2,P2xpos,P2ypos),nl,endGame(_);
          \+isWinCondition(X2,P2xpos,P2ypos), playGame(X2)) ).


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


