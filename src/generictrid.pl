:- use_module(library(clpfd)).
:- use_module(library(random)).

%playGameauto([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,12,B1,C1,9,D1,E1,F1,6,6,G1,12,H1,8,I1,P1,1,5]).



%tab 5*5
genericChoose:- 
        write('Choose the Game Size: '),nl,
        read(TridSize),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerGeneric(MinValue,MaxValue,TridSize).



tridplayerGeneric(MinValue,MaxValue,TridSize) :-
        generateVariablesList(TridSize,VariablesList,1),
        TridSizemenos1 is TridSize - 1,
        generateTrianglesList(TridSizemenos1,TrianglesList,1),
        Values = [MinValue,MaxValue],
        playGameGenericAuto(VariablesList,TrianglesList,Values,TridSize),
        randomvalues(R1,R2,R3,R4,TridSize),
        nth1(R1,TrianglesList,Valor1),
        nth1(R2,TrianglesList,Valor2),
        nth1(R3,TrianglesList,Valor3),
        nth1(R4,TrianglesList,Valor4),!,
        generateVariablesList(TridSize,VariablesListPlay,1),
        TridSizemenos1 is TridSize - 1,
        generateTrianglesList(TridSizemenos1,TrianglesListPlay,1),
        
        
        replaceInThePosition(TrianglesListPlay, R1, Valor1, RList),
        replaceInThePosition(TrianglesListPlay, R2, Valor2, RList),
        replaceInThePosition(TrianglesListPlay, R3, Valor3, RList),
        replaceInThePosition(TrianglesListPlay, R4, Valor4, RList),
        write('A gerar problema...'),nl,nl,   
        sleep(1),
        generateBoard(X1,VariablesListPlay,RList),
        printBoard(X1),   
        write('A resolver o problema...'),nl,nl,   
        sleep(1),
        playGameGeneric(VariablesListPlay,RList,Values,TridSize),
        generateBoard(X,VariablesListPlay,RList),
        printBoard(X).






playGameGenericAuto([Vertices],[Triangles],[MinValue,MaxValue], TamanhoTrid) :-
        domain(Vertices,MinValue,MaxValue),
        %        lineRestrictions(Vertices,Triangles),
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


%test:- generateVariablesList(5,VariablesList,1), write(VariablesList).
generateVariablesList(X,[Head|[]],X):-length(Head,X).
generateVariablesList(TridSize,[Head|Tail],N):- length(Head,N),
        N1 is N + 1,
        generateVariablesList(TridSize,Tail,N1).

%test:- generateTrianglesList(4,VariablesList,1), write(VariablesList).
generateTrianglesList(X,[Head|[]],X):- Nimpar is X - 1,
        Size is 2*Nimpar + 1,
        length(Head,Size).

generateTrianglesList(TridSizemenos1,[Head|Tail],1):- length(Head,1),
        generateTrianglesList(TridSizemenos1,Tail,2).

generateTrianglesList(TridSizemenos1,[Head|Tail],N):- Nimpar is N - 1,
        Size is 2*Nimpar + 1,
        length(Head,Size),
        N1 is N + 1,
        generateTrianglesList(TridSizemenos1,Tail,N1).

