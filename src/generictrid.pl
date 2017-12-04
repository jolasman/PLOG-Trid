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
        playGameGeneric(List),
        generateBoard(X,List),
        printBoard(X).


playGameGenericAuto([Vertices],[Triangles],[MinValue,MaxValue], TamanhoTrid) :-
        domain(Vertices,MinValue,MaxValue),
        lineRestrictions(Vertices,Triangles),
        TamanhoTridMenos1 is TamanhoTrid - 1,
        restricoes(Vertices,Triangles,TamanhoTridMenos1),
        append(Vertices,Triangles,Result),
        labeling([],[Result]).

%lineRestrictions(Vertices,Triangles):-


%    restricoes([['A'],['B','C'],['D','E','F'],['G','H','I','J']],[['A1'],['B1','C1','D1'],['E1','F1','G1','H1','I1']],3).    
%    restricoes([['A'],['B','C'],['D','E','F'],['G','H','I','J'], ['K','L','M','N','O']],[['A1'],['B1','C1','D1'],['E1','F1','G1','H1','I1'], ['J1','k1','L1','M1','N1','O1','P1']],4).    
restricoes(_,_,0).
restricoes(Vertices,Triangles,TamanhoTridMenos1):- restrictions(Vertices,Triangles,TamanhoTridMenos1),
        Aux is TamanhoTridMenos1 - 1,
        restricoes(Vertices,Triangles,Aux).

%       restrictions([['A'],['B','C'],['D','E','F'],['G','H','I','J']],[['A1'],['B1','C1','D1'],['E1','F1','G1','H1','I1']],2).    
restrictions([Hvertices,H2ver|_],[Htriangles|_], 1):- 
        nth1(1,Hvertices,A),
        nth1(1,H2ver,B),
        nth1(2,H2ver,C),
        nth1(1,Htriangles,A1),
        A+B+C #= A1.

restrictions(Vertices,Triangles, N):- write('entrou'),
        nth1(N,Vertices,ListaVerticesN),
        N1 is N + 1,
        nth1(N1,Vertices,ListaVerticesN1),
        nth1(N,Triangles,ListaTriangles),
        length(ListaTriangles,X),
        restrictionsAux(ListaVerticesN,ListaVerticesN1, ListaTriangles,X, 1).   

restrictionsAux([],[],[],_,_).
restrictionsAux([A,B|TVerticesN],[C,D|TVerticesN1], [A1,B1|TListaTriangles],X, Tinicial):- Tinicial < X,
%        A+C+D #= A1,
%        A+B+D #= B1,
        Tinicial1 is Tinicial + 2,

        write(Tinicial1),nl,
        write(X),nl,
        
        write(A),
        write('+'),
        write(C),
        write('+'),
        write(D),
        write('='),
        write(A1),nl,nl,
        write(A),
        write('+'),
        write(B),
        write('+'),
        write(D),
        write('='),
        write(B1),nl,nl,

        restrictionsAux([B|TVerticesN],[D|TVerticesN1],TListaTriangles,X, Tinicial1).

restrictionsAux([A|_],[C,D|_], [A1|_],X, Tinicial):- Tinicial == X,
%        A+C+D #= A1,
        write('auxfinal'),nl,
        write(A),
        write('+'),
        write(C),
        write('+'),
        write(D),
        write('='),
        write(A1),nl,nl.




