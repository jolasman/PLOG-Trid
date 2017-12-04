:- use_module(library(clpfd)).
:- use_module(library(random)).

%playGameauto([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1,1,5]).

%tab 5*5
autoChoose5:-printBoard5(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto5(MinValue,MaxValue).



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
        write('A gerar problema...'),nl,nl,   
        sleep(1),
        generateBoard5(X1,List),
        printBoard5(X1),   
        write('A resolver o problema...'),nl,nl,   
        sleep(1),
        playGame5(List),
        generateBoard5(X,List),
        printBoard5(X).

/*
   L is the current list,
   P is the position where the element will be insert,
   E is the element to insert and
   R is the return, the new list
*/
replaceInThePosition(L, P, E, R) :-
        findall(X, (nth0(I,L,Y), (I == P -> X=E ; X=Y)), R).


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


randomvalues(R1,R2,R3,R4, TridSize):- N is TridSize - 1,
        Final is 2*N + 1,
        random(1,Final, R1),
        random(1,Final, R2),
        random(1,Final, R3),
        random(1,Final, R4),
        all_different([R1,R2,R3,R4]),
        labeling([],[R1,R2,R3,R4]).

