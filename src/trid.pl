/*******************************
   * para correr o jogo e' consultar este ficheiro e fazer start(X). *
   *******************************/

% tridplayer([0,12,0,0,9,0,0,0,6,6,0,12,0,8,0,0]).
% tridplayer([A,12,B,C,9,D,E,F,6,6,G,12,H,8,I,P]).

:- include('print.pl').
:- include('tridauto.pl').
:- include('board.pl').
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
menusizesManual(Choice):- Choice == 1, 
        tridchoose4. 
menusizesManual(Choice):- Choice == 2, 
        tridchoose5.
menusizesManual(Choice):- Choice == 3, 
        tridchoose7. 
menusizesManual(Choice):- Choice == 4, 
        tridchoose8.
menusizesManual(Choice):- Choice == 5, 
        exit(_).

% menu automatico
menusizesAuto(Choice):- Choice == 1, 
        autoChoose4. 
menusizesAuto(Choice):- Choice == 2, 
        autoChoose5.
menusizesAuto(Choice):- Choice == 3, 
        autoChoose7. 
menusizesAuto(Choice):- Choice == 4, 
        autoChoose8.
menusizesAuto(Choice):- Choice == 5, 
        exit(_).

%tab 4*4
tridchoose4:- generateEmptyBoard4(X),
        printBoard4(X),
        write('Escolha as variaveis em forma de lista. Ex: [A1,12,B1,C1,9,D1,E1,F1,6] : '),nl,
        read(Choice),nl,
        write('Escolha o valor minimo do dominio de A..O : '),nl,
        read(MinValue),nl,
        write('Escolha o valor maximo do dominio de A..O : '),nl,
        read(MaxValue),nl,
        tridplayer4(Choice,MinValue,MaxValue).


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

%tab 7*7
tridchoose7:- generateEmptyBoard7(X),
        printBoard7(X),
        write('Escolha as variaveis em forma de lista. Ex: [A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1] : '),nl,
        read(Choice),nl,
        write('Escolha o valor minimo do dominio de A..O : '),nl,
        read(MinValue),nl,
        write('Escolha o valor maximo do dominio de A..O : '),nl,
        read(MaxValue),nl,
        tridplayer7(Choice,MinValue,MaxValue).


%tab 8*8
tridchoose8:- generateEmptyBoard8(X),
        printBoard8(X),
        write('Escolha as variaveis em forma de lista. Ex: [A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1] : '),nl,
        read(Choice),nl,
        write('Escolha o valor minimo do dominio de A..O : '),nl,
        read(MinValue),nl,
        write('Escolha o valor maximo do dominio de A..O : '),nl,
        read(MaxValue),nl,
        tridplayer8(Choice,MinValue,MaxValue).


tridplayer4(Choice,MinValue,MaxValue) :-
        variables4(A1,B1,C1,D1,E1,F1,G1,H1,I1,Choice),
        playGame4([A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1,MinValue,MaxValue]),
        generateBoard4(X,A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1),
        printBoard4(X).

tridplayer5(Choice,MinValue,MaxValue) :-
        variables5(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Choice),
        playGame5([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,MinValue,MaxValue]),
        generateBoard5(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1),
        printBoard5(X).

tridplayer7(Choice,MinValue,MaxValue) :-
        variables7(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,
                   AA1,BB1,CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1,Choice),
        playGame7([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
                   G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                   HH1,II1,JJ1,MinValue,MaxValue]),
        generateBoard7(XBoard,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
                       G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                       HH1,II1,JJ1),
        printBoard7(XBoard).

tridplayer8(Choice,MinValue,MaxValue) :-
        variables8(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,
                   CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1,Choice),
        playGame8([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
                   G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                   HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1,MinValue,MaxValue]),
        generateBoard8(XBoard,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
                       G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                       HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1),
        printBoard8(XBoard).

variables4(A1,B1,C1,D1,E1,F1,G1,H1,I1,Choice):- 
        nth1(1,Choice,A1),
        nth1(2,Choice,B1),
        nth1(3,Choice,C1),
        nth1(4,Choice,D1),
        nth1(5,Choice,E1),
        nth1(6,Choice,F1),
        nth1(7,Choice,G1),
        nth1(8,Choice,H1),
        nth1(9,Choice,I1).

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

variables7(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,
           AA1,BB1,CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1,Choice):- 
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
        nth1(16,Choice,P1),
        nth1(17,Choice,Q1),
        nth1(18,Choice,R1),
        nth1(19,Choice,S1),
        nth1(20,Choice,T1),
        nth1(21,Choice,U1),
        nth1(22,Choice,V1),
        nth1(23,Choice,W1),
        nth1(24,Choice,X1),
        nth1(25,Choice,Y1),
        nth1(26,Choice,Z1),
        nth1(27,Choice,AA1),
        nth1(28,Choice,BB1),
        nth1(29,Choice,CC1),
        nth1(30,Choice,DD1),
        nth1(31,Choice,EE1),
        nth1(32,Choice,FF1),
        nth1(33,Choice,GG1),
        nth1(34,Choice,HH1),
        nth1(35,Choice,II1),
        nth1(36,Choice,JJ1).


variables8(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,
           CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1,Choice):- 
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
        nth1(16,Choice,P1),
        nth1(17,Choice,Q1),
        nth1(18,Choice,R1),
        nth1(19,Choice,S1),
        nth1(20,Choice,T1),
        nth1(21,Choice,U1),
        nth1(22,Choice,V1),
        nth1(23,Choice,W1),
        nth1(24,Choice,X1),
        nth1(25,Choice,Y1),
        nth1(26,Choice,Z1),
        nth1(27,Choice,AA1),
        nth1(28,Choice,BB1),
        nth1(29,Choice,CC1),
        nth1(30,Choice,DD1),
        nth1(31,Choice,EE1),
        nth1(32,Choice,FF1),
        nth1(33,Choice,GG1),
        nth1(34,Choice,HH1),
        nth1(35,Choice,II1),
        nth1(36,Choice,JJ1),
        nth1(37,Choice,KK1),
        nth1(38,Choice,LL1),
        nth1(39,Choice,MM1),
        nth1(40,Choice,NN1),
        nth1(41,Choice,OO1),
        nth1(42,Choice,PP1),
        nth1(43,Choice,QQ1),
        nth1(44,Choice,RR1),
        nth1(45,Choice,SS1),
        nth1(46,Choice,TT1),
        nth1(47,Choice,UU1),
        nth1(48,Choice,VV1),
        nth1(49,Choice,WW1).

playGame4([A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J],MinValue,MaxValue),
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
        labeling([],[A,B,C,D,E,F,G,H,I,J]).

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


playGame7([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
           G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
           HH1,II1,JJ1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB],MinValue,MaxValue),
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
        K + P + Q #= Q1,
        K + L + Q #= R1,
        L + Q + R #= S1,
        L + R + M #= T1,
        M+R+S #= U1,
        M+N+S #= V1,
        S+N+T #= W1,
        N+O+T #= X1,
        T+O+U #= Y1,
        V+P+W #= Z1,
        P+W+Q #= AA1,
        W+Q+X #= BB1,
        Q+R+X #= CC1,
        X+Y+R #= DD1,
        R+S+Y #= EE1,
        Y+S+Z #= DD1,
        R+S+Y #= EE1,
        Y+S+Z #= FF1,
        S+Z+T #= GG1,
        Z+T+AA #= HH1,
        T+U+AA #= II1,
        U+AA+BB #= JJ1,
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB]).

playGame8([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
                   G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                   HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ],MinValue,MaxValue),
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
        K + P + Q #= Q1,
        K + L + Q #= R1,
        L + Q + R #= S1,
        L + R + M #= T1,
        M+R+S #= U1,
        M+N+S #= V1,
        S+N+T #= W1,
        N+O+T #= X1,
        T+O+U #= Y1,
        V+P+W #= Z1,
        P+W+Q #= AA1,
        W+Q+X #= BB1,
        Q+R+X #= CC1,
        X+Y+R #= DD1,
        R+S+Y #= EE1,
        Y+S+Z #= DD1,
        R+S+Y #= EE1,
        Y+S+Z #= FF1,
        S+Z+T #= GG1,
        Z+T+AA #= HH1,
        T+U+AA #= II1,
        U+AA+BB #= JJ1,
        V+CC+DD #= KK1,
        V+W+DD #= LL1,
        W+DD+EE #= MM1,
        W+X+EE #= NN1,
        X+EE+FF #= OO1,
        X+Y+FF #= PP1,
        Y+FF+GG #= QQ1,
        Y+GG+Z #= RR1,
        Z+GG+HH #= SS1,
        Z+AA+HH #= TT1,
        AA+HH+II #= UU1,
        AA+II+BB #= VV1,
        BB+II+JJ #= WW1,       
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ]).

endGame(_):- 
        write('****************************************************'),nl,
        write('*************** Trid Version 1.0 *******************'),nl,
        write('****************************************************'),nl,
        nl,nl,nl.

exit(_) :- nl,nl,write('See you later!!!!'),nl,nl.
