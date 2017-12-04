:- use_module(library(clpfd)).
:- use_module(library(random)).

%playGameauto([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1,1,5]).



%tab 5*5
autoChoose:- generateEmptyBoard(X),
        printBoard(X),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerauto(MinValue,MaxValue).



% gera um numero aleatorio de entre a lista de A1..P1, depois resolve o problema com base nesse valor


tridplayerauto(MinValue,MaxValue) :-
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
        generateBoard(X1,List),
        printBoard(X1),   
        write('A resolver o problema...'),nl,nl,   
        sleep(1),
        playGame(List),
        generateBoard(X,List),
        printBoard(X).



/*
   L is the current list,
   P is the position where the element will be insert,
   E is the element to insert and
   R is the return, the new list
*/



playGameGeneric([Vertices],[Triangles],[MinValue,MaxValue]) :-
        domain(Vertices,MinValue,MaxValue),







        append(Vertices,Triangles,Result),
        labeling([],[Result]).

