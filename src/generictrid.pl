:- use_module(library(clpfd)).
:- use_module(library(random)).

genericChoose:- 
        write('Choose the Game Size: '),nl,
        read(TridSize),
        write('Choose the MinValue: '),nl,
        read(MinValue),
        write('Choose the MaxValue: '),nl,
        read(MaxValue),
        tridplayerGeneric(MinValue,MaxValue,TridSize).

tridplayerGeneric(MinValue,MaxValue,TridSize) :-
        count(TridSize,Total),
        length(VariablesList,Total),
        length(VariablesList1,Total),
        TridSizemenos1 is TridSize - 1,
        countp(TridSizemenos1,TotalTri),
        length(TrianglesList,TotalTri),
        Values = [MinValue,MaxValue],

        playGameGenericAuto(VariablesList,TrianglesList,Values,TridSize),
        write(TrianglesList),nl,

        randomvalues(R1,R2,R3,R4,TotalTri),
        nth1(R1,TrianglesList,Valor1),
        nth1(R2,TrianglesList,Valor2),
        nth1(R3,TrianglesList,Valor3),
        nth1(R4,TrianglesList,Valor4),
        update(R1,R2,R3,R4,C1,C2,C3,C4),

        length(VariablesListPlay,Total),
        length(TrianglesListPlay,TotalTri),
        replaceInThePosition(TrianglesListPlay, C1, Valor1, RList),
        replaceInThePosition(TrianglesListPlay, C2, Valor2, RList),
        replaceInThePosition(TrianglesListPlay, C3, Valor3, RList),
        replaceInThePosition(TrianglesListPlay, C4, Valor4, RList),
        generateTrianglesListFrom(RList,1,RT,TridSizemenos1),
        write('A gerar problema...'),nl,nl,   
        sleep(1),
        generateVariablesListFrom(VariablesList1,1,RV,TridSize),
        printBoard(RV,RT),   
        write('A resolver o problema...'),nl,nl,   
        sleep(1),!,
        playGameGeneric(VariablesListPlay,RList,Values,TridSize),
        generateVariablesListFrom(VariablesListPlay,1,RV1,TridSize),
        generateTrianglesListFrom(RList,1,RT1,TridSizemenos1),
        printBoard(RV1,RT1).

playGameGenericAuto(Vertices,Triangles,[MinValue,MaxValue], TamanhoTrid) :-
        domain(Vertices,MinValue,MaxValue),
        TamanhoTridMenos1 is TamanhoTrid - 1,
        generateVariablesListFrom(Vertices,1,RV,TamanhoTrid),
        generateTrianglesListFrom(Triangles,1,RT,TamanhoTridMenos1),
        write(RV),nl,
        write(RT),nl,
        restricoes(RV,RT,TamanhoTridMenos1),
        lineRestrictions(RV,TamanhoTrid),
        labeling([],Vertices).

playGameGeneric(Vertices,Triangles,[MinValue,MaxValue], TamanhoTrid) :- write('play generic'),nl,
        domain(Vertices,MinValue,MaxValue),
        TamanhoTridMenos1 is TamanhoTrid - 1,
        generateVariablesListFrom(Vertices,1,RV,TamanhoTrid),
        generateTrianglesListFrom(Triangles,1,RT,TamanhoTridMenos1),
        write('estou a fazer'),nl,
        write(RV),nl,
        write(RT),nl,
        restricoes(RV,RT,TamanhoTridMenos1),
        lineRestrictions(RV,TamanhoTrid),
        write('fez restricoes'),nl,!,
        labeling([],Vertices).



%------------------------

lineRestrictions(Vertices,TridSize):- innerRestrictions(Vertices,2,2,TridSize),
        allfirsts(Vertices,ResultF),
        all_different(ResultF),
        allLinesDistinct(Vertices),
        allfinals(Vertices,ResultFinals),
        all_different(ResultFinals).



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
        A+C+D #= A1,
        A+B+D #= B1,
        Tinicial1 is Tinicial + 2,
        restrictionsAux([B|TVerticesN],[D|TVerticesN1],TListaTriangles,X, Tinicial1).

restrictionsAux([A|_],[C,D|_], [A1|_],X, Tinicial):- Tinicial == X,
        A+C+D #= A1.


%c:- allfirsts([['A'],['B','C'],['D','E','F'],['G','H','I','J'], ['K','L','M','N','O']], Rs), write(Rs).
allfirsts(Ls, Rs) :-
        maplist(member, Rs, Ls).

%l:- allLinesDistinct([['A'],['B','C'],['D','E','F'],['G','H','I','J'], ['K','L','M','N','O']]).
allLinesDistinct([]).
allLinesDistinct([Head|Tail]):- all_different(Head),
        allLinesDistinct(Tail). 


%f:- allfinals([['A'],['B','C'],['D','E','F'],['G','H','I','J'], ['K','L','M','N','O']], Rs), write(Rs).
allfinals([],[]).
allfinals([Head|Tail],[H|T]):-
        length(Head,SizeH),
        nth1(SizeH,Head,H),
        allfinals(Tail,T).

%d:- diagonal1([['A'],['B','C'],['D','E','F'],['G','H','I','J'],['K','L','M','N','O']],2,1,5, Rs), write(Rs).
diagonal1(Vertices,X,Y,TridSize,[H|[]]):- X == TridSize,
        nth1(X,Vertices,ListaR),
        nth1(Y,ListaR,H).
diagonal1(Vertices,X,Y,TridSize,[H|T]):- X < TridSize,
        nth1(X,Vertices,ListaR),
        nth1(Y,ListaR,H),
        X1 is X + 1,
        Y1 is Y + 1,
        diagonal1(Vertices,X1,Y1,TridSize, T).

%d:- diagonal2([['A'],['B','C'],['D','E','F'],['G','H','I','J'],['K','L','M','N','O']],2,1,5, Rs), write(Rs).
diagonal2(Vertices,X,Y,TridSize,[H|[]]):- X == TridSize,
        nth1(X,Vertices,ListaR),
        nth1(Y,ListaR,H).
diagonal2(Vertices,X,Y,TridSize,[H|T]):- X < TridSize,
        nth1(X,Vertices,ListaR),
        nth1(Y,ListaR,H),
        X1 is X + 1,
        diagonal2(Vertices,X1,Y,TridSize, T).


%i:- innerRestrictions([['A'],['B','C'],['D','E','F'],['G','H','I','J'],['K','L','M','N','O']],2,2,5).
innerRestrictions(Vertices,X,Y,TridSize):- Tridsizemenos1 is TridSize - 1,
        X == Tridsizemenos1,
        diagonal1(Vertices,X,1,TridSize,ResultList),
        all_different(ResultList),
        diagonal2(Vertices,X,Y,TridSize,ResultList2),
        all_different(ResultList2).
innerRestrictions(Vertices,X,Y,TridSize):- Tridsizemenos1 is TridSize - 1,
        X < Tridsizemenos1,
        diagonal1(Vertices,X,1,TridSize,ResultList),
        all_different(ResultList),
        diagonal2(Vertices,X,Y,TridSize,ResultList2),
        all_different(ResultList2),
        X1 is X + 1,
        Y1 is Y + 1,
        innerRestrictions(Vertices,X1,Y1,TridSize).


%----------------------------

%generateVariablesListFrom([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],1,R,5).
generateVariablesListFrom([], _, [],_).
generateVariablesListFrom(L, N, [DL|DLTail],Size) :-
        length(DL, N),
        append(DL, LTail, L),
        N1 is N + 1,
        generateVariablesListFrom(LTail, N1, DLTail,Size).

generateTrianglesListFrom([], _, [],_).
generateTrianglesListFrom(L, 1, [DL|DLTail],Size) :-
        length(DL, 1),
        append(DL, LTail, L),
        generateTrianglesListFrom(LTail, 2, DLTail,Size).

%generateTrianglesListFrom([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],1,R,4).
generateTrianglesListFrom(L, N, [DL|DLTail],Sizemenos1) :-
        N1 is N - 1,
        SizeT is 2*N1 + 1,     
        length(DL, SizeT),
        append(DL, LTail, L),
        NR is N + 1,
        generateTrianglesListFrom(LTail, NR, DLTail,Sizemenos1).

%----------------------------------


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

count(0,0).
count(TridSize,R):- New is TridSize - 1,
        count(New,R1),
        R is R1 + TridSize.

countp(0,0).
countp(TridSizemenos1,R):- New is TridSizemenos1 - 1,
        countp(New,R1),
        R is R1 + 2*New +1.

update(R1,R2,R3,R4,C1,C2,C3,C4):-
        C1 is R1 - 1,
        C2 is R2 - 1,
        C3 is R3 - 1,
        C4 is R4 - 1.
