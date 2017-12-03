:- use_module(library(clpfd)).
:- use_module(library(random)).

%playGameauto([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1,1,5]).

%tab 4*4
autoChoose4:- generateEmptyBoard4(X),
        printBoard4(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto4(MinValue,MaxValue).

%tab 5*5
autoChoose5:- generateEmptyBoard5(X),
        printBoard5(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto5(MinValue,MaxValue).

%tab 7*7
autoChoose7:- generateEmptyBoard7(X),
        printBoard7(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto7(MinValue,MaxValue).

%tab 5*5
autoChoose8:- generateEmptyBoard8(X),
        printBoard8(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto8(MinValue,MaxValue).

% gera um numero aleatorio de entre a lista de A1..P1, depois resolve o problema com base nesse valor
tridplayerauto4(MinValue,MaxValue) :-
        length(List19,19),
        Values = [MinValue,MaxValue],
        append(List19,Values,Listaauto),
        playGameauto4(Listaauto),
        randomvalues(R1,R2,_,_),
        nth1(R1,Listaauto,Valor1),
        nth1(R2,Listaauto,Valor2),!,
        length(List1,10),
        length(List2,9),
        List3 = [MinValue,MaxValue],
        R1P is R1 - 9,
        R2P is R2 - 9,
        replaceInThePosition(List2, R1P, Valor1, RList),
        replaceInThePosition(List2, R2P, Valor2, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write('A gerar problema...'),nl,   
        sleep(1),
        generateBoard4(X1,List),
        printBoard4(X1),   
        write('A resolver o problema...'),nl,   
        sleep(1),
        playGame4(List),
        generateBoard4(X,List),
        printBoard4(X).

tridplayerauto5(MinValue,MaxValue) :-
        length(List31,31),
        Values = [MinValue,MaxValue],
        append(List31,Values,Listaauto),
        playGameauto5(Listaauto),
        randomvalues(R1,R2,R3,R4),
        nth1(R1,Listaauto,Valor1),
        nth1(R2,Listaauto,Valor2),
        nth1(R3,Listaauto,Valor3),
        nth1(R4,Listaauto,Valor4),!,
        length(List1,15),
        length(List2,16),
        List3 = [MinValue,MaxValue],
        R1P is R1 - 16,
        R2P is R2 - 16,
        R3P is R3 - 16,
        R4P is R4 - 16,
        replaceInThePosition(List2, R1P, Valor1, RList),
        replaceInThePosition(List2, R2P, Valor2, RList),
        replaceInThePosition(List2, R3P, Valor3, RList),
        replaceInThePosition(List2, R4P, Valor4, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write('A gerar problema...'),nl,   
        sleep(1),
        generateBoard5(X1,List),
        printBoard5(X1),   
        write('A resolver o problema...'),nl,   
        sleep(1),
        playGame5(List),
        generateBoard5(X,List),
        printBoard5(X).

randomvalues(R1,R2,R3,R4):- random(16,30, R1),
        random(16,30, R2),
        random(16,30, R3),
        random(16,30, R4),
        all_different([R1,R2,R3,R4]),
        labeling([],[R1,R2,R3,R4]).

tridplayerauto7(MinValue,MaxValue) :-
        length(List1,28),
        length(List2,36),
        List3 = [MinValue,MaxValue],
        random(3,25,R),
        random(1,36,Index),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,   
        sleep(1),
        playGameauto7(List),
        generateBoard7(X,List),
        printBoard7(X).

tridplayerauto8(MinValue,MaxValue) :-
        length(List1,36),
        length(List2,49),
        List3 = [MinValue,MaxValue],
        random(3,31,R),
        random(1,49,Index),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,  
        sleep(1), 
        playGameauto8(List),
        generateBoard8(X,List),
        printBoard8(X).


/*
   L is the current list,
   P is the position where the element will be insert,
   E is the element to insert and
   R is the return, the new list
*/
replaceInThePosition(L, P, E, R) :-
        findall(X, (nth0(I,L,Y), (I == P -> X=E ; X=Y)), R).

playGameauto4([A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J],MinValue,MaxValue),
        all_different([A,B,D,G]),
        all_different([A,C,F,J]),
        all_different([G,H,I,J]),
        all_different([D,E,F]),
        all_different([B,C]),
        all_different([B,E,I]),
        all_different([D,H]),
        all_different([C,E,H]),
        all_different([F,I]),
        A + B + C #= A1, 
        D + B + E #= B1,
        C + B + E #= C1,
        C + F + E #= D1,
        G + D + H #= E1,
        D + E + H #= F1,
        H + E + I #= G1,
        I + F + E #= H1,
        I + J + F #= I1,   
        labeling([],[A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1]).

playGameauto5([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,MinValue,MaxValue]) :-
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
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1]).

playGameauto7([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
               G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
               HH1,II1,JJ1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB],MinValue,MaxValue),
        all_different([A,B,D,G,K,P,V]),
        all_different([A,C,F,J,O,U,BB]),
        all_different([K,L,M,N,O]),
        all_different([G,H,I,J]),
        all_different([D,E,F]),
        all_different([B,C]),
        all_different([P,Q,R,S,T,U]),
        all_different([V,W,X,Y,Z,AA,BB]),
        all_different([B,E,I,N,T,AA]),
        all_different([D,H,M,S,Z]),
        all_different([G,L,R,Y]),
        all_different([K,Q,X]),
        all_different([P,W]),
        all_different([C,E,H,L,Q,W]),
        all_different([F,I,M,R,X]),
        all_different([J,N,S,Y]),
        all_different([O,T,Z]),
        all_different([U,AA]),

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
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
                     G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                     HH1,II1,JJ1]).

playGameauto8([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
               G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
               HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1,MinValue,MaxValue]) :-
        domain([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ],MinValue,MaxValue),
        all_different([A,B,D,G,K,P,V,CC]),
        all_different([A,C,F,J,O,U,BB,JJ]),
        all_different([K,L,M,N,O]),
        all_different([G,H,I,J]),
        all_different([D,E,F]),
        all_different([B,C]),
        all_different([P,Q,R,S,T,U]),
        all_different([V,W,X,Y,Z,AA,BB]),
        all_different([CC,DD,EE,FF,GG,HH,II,JJ]),
        all_different([B,E,I,N,T,AA,II]),
        all_different([D,H,M,S,Z,HH]),
        all_different([G,L,R,Y,GG]),
        all_different([K,Q,X,FF]),
        all_different([P,W,EE]),
        all_different([V,DD]),
        all_different([C,E,H,L,Q,W,DD]),
        all_different([F,I,M,R,X,EE]),
        all_different([J,N,S,Y,FF]),
        all_different([O,T,Z,GG]),
        all_different([U,AA,HH]),
        all_different([BB,II]),
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
        labeling([],[B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
                     G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,DD1,EE1,FF1,GG1,
                     HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1]).
