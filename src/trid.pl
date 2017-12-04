/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

% tridplayer([A,12,B,C,9,D,E,F,6]).
% tridplayer([A,12,B,C,9,D,E,F,6,6,G,12,H,8,I,P]).

:- include('print.pl').
:- include('tridauto.pl').
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
        nl, write('1 - Play Game choosing the variables'),
        nl, write('2 - Play Game in an automatic way'),
        nl, write('3 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(Choice),
        menu(Choice).

menu(Choice):- Choice == 1, 
        write('***************************************************'),nl,
        write('***************  Trid - Sizes   *******************'),nl,
        write('***************************************************'),nl,
        nl,nl,
        nl, write('1 - 4 * 4'),
        nl, write('2 - 5 * 5'),
        nl, write('3 - 7 * 7'),
        nl, write('4 - 8 * 8'),
        nl, write('5 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(ChoiceManual),
        menusizesManual(ChoiceManual).

menu(Choice):- Choice == 2, 
        write('***************************************************'),nl,
        write('***************  Trid - Sizes   *******************'),nl,
        write('***************************************************'),nl,
        nl,nl,
        nl, write('1 - 4 * 4'),
        nl, write('2 - 5 * 5'),
        nl, write('3 - 7 * 7'),
        nl, write('4 - 8 * 8'),
        nl, write('5 - Exit Game'),nl,
        write('Choose : '),nl,nl,
        read(ChoiceAuto),
        menusizesAuto(ChoiceAuto).
menu(Choice):- Choice == 3, 
        exit(_).

% menu com input

menusizesManual(Choice):- Choice == 2, 
        tridchoose5.

menusizesManual(Choice):- Choice == 5, 
        exit(_).

% menu automatico

menusizesAuto(Choice):- Choice == 2, 
        autoChoose5.
menusizesAuto(Choice):- Choice == 5, 
        exit(_).


%tab 5*5
tridchoose5:- generateEmptyBoard5(X),
        printBoard5(X),
        write('Escolha as variaveis em forma de lista. Ex: [A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1] : '),nl,
        read(Choice),nl,
        write('Escolha o valor minimo do dominio de A..O : '),nl,
        read(MinValue),nl,
        write('Escolha o valor maximo do dominio de A..O : '),nl,
        read(MaxValue),nl,
        tridplayer5(Choice,MinValue,MaxValue).


tridplayer5(Choice,MinValue,MaxValue) :-
        variables5(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Choice),
        write('A resolver o problema...'),nl,
        sleep(1),
        playGame5([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,MinValue,MaxValue]),
        generateBoard5(X,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,MinValue,MaxValue]),
        printBoard5(X).

variables5(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Choice):- 
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


playGame5([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],MinValue,MaxValue),
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


endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Trid Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.
