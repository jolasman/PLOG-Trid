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
        List1 = [A,B,C,D,E,F,G,H,I,J],
        List2 = [A1,B1,C1,D1,E1,F1,G1,H1,I1],
        List3 = [MinValue,MaxValue],
        random_member(Elem,List2),
        random(3,9,R),
        nth1(Index,List2,Elem),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,   
        sleep(2),
        playGameauto4(List),
        generateBoard4(X,A,B,C,D,E,F,G,H,I,J,A1,B1,C1,D1,E1,F1,G1,H1,I1),
        printBoard4(X).

tridplayerauto5(MinValue,MaxValue) :-
        List1 = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
        List2 = [A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1],
        List3 = [MinValue,MaxValue],
        random_member(Elem,List2),
        random(3,16,R),
        nth1(Index,List2,Elem),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,   
        sleep(2),
        playGameauto5(List),
        generateBoard5(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1),
        printBoard5(X).

tridplayerauto7(MinValue,MaxValue) :-
        List1 = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB],
        List2 = [A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,
                 CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1],
        List3 = [MinValue,MaxValue],
        random_member(Elem,List2),
        random(3,25,R),
        nth1(Index,List2,Elem),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,   
        sleep(2),
        playGameauto7(List),
        generateBoard7(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,
                       A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,
                       BB1,CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1),
        printBoard7(X).

tridplayerauto8(MinValue,MaxValue) :-
        List1 = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ],
        List2 = [A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,BB1,CC1,
                 DD1,EE1,FF1,GG1,HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1],
        List3 = [MinValue,MaxValue],
        random_member(Elem,List2),
        random(3,31,R),
        nth1(Index,List2,Elem),
        replaceInThePosition(List2, Index, R, RList),
        append(List1,RList,LR),       
        append(LR,List3,List), !,
        write(RList),nl,   
        write('A resolver o problema...'),nl,   
        sleep(2),
        playGameauto8(List),
        generateBoard8(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,
                       A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1,P1,Q1,R1,S1,T1,U1,V1,W1,X1,Y1,Z1,AA1,
                       BB1,CC1,DD1,EE1,FF1,GG1,HH1,II1,JJ1, KK1,LL1,MM1,NN1,OO1,PP1,QQ1,RR1,SS1,TT1,UU1,VV1,WW1),
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
        labeling([],[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]).

playGameauto7([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,A1,B1,C1,D1,E1,F1,
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

playGameauto8([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,AA,BB,CC,DD,EE,FF,GG,HH,II,JJ,A1,B1,C1,D1,E1,F1,
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

randomList(Zs,Length,MinValue,MaxValue) :- length(Zs, Length), maplist(random(MinValue,MaxValue), Zs).
