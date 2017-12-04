/***************************
* OUTPUT RELATED FUNCTIONS *
***************************/


/*Printing an entire board 5*5*/
printBoard5([]).
printBoard5([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :- 
        printLine1(X1),
        printLine2(X2),
        printLine3(X3),
        printLine4(X4),
        printLine5(X5),
        printLine6(X6),
        printLine7(X7),
        printLine8(X8),
        printLine9(X9).


printLiteralLine([X|Xs]) :- write(X), printLiteralLine(Xs).

printLine1([X|Xs]) :- write('          /'), printLine1aux([X|Xs]).
printLine1aux([]) :- write('\\ \n'),nl.
printLine1aux([X|Xs]) :- X \== 'E', write(X), printLine1aux(Xs).
printLine1aux([X|Xs]) :- X == 'E', printLine1aux(Xs).

printLine2([X|Xs]) :- write('         /'), printLine2aux([X|Xs]).
printLine2aux([]) :- write('\\ \n'),nl.
printLine2aux([X|Xs]) :- X \== 'E', write(X), printLine2aux(Xs).
printLine2aux([X|Xs]) :- X == 'E', printLine2aux(Xs).

printLine3([X|Xs]) :- write('        /'), printLine3aux([X|Xs]).
printLine3aux([]) :- write('\\ \n'),nl.
printLine3aux([X|Xs]) :- X \== 'E', write(X), write(' '), printLine3aux(Xs).
printLine3aux([X|Xs]) :- X == 'E', printLine3aux(Xs).

printLine4([X|Xs]) :- write('       /'), printLine4aux([X|Xs]).
printLine4aux([]) :- write('\\ \n'),nl.
printLine4aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine4aux(Xs).
printLine4aux([X|Xs]) :- X == 'E', printLine4aux(Xs).

printLine5([X|Xs]) :- write('      /'), printLine5aux([X|Xs]).
printLine5aux([]) :- write('\\ \n'),nl.
printLine5aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine5aux(Xs).
printLine5aux([X|Xs]) :- X == 'E', printLine5aux(Xs).

printLine6([X|Xs]) :- write('     /'), printLine6aux([X|Xs]).
printLine6aux([]) :- write('\\ \n'),nl.
printLine6aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine6aux(Xs).
printLine6aux([X|Xs]) :- X == 'E', printLine6aux(Xs).

printLine7([X|Xs]) :- write('    /'), printLine7aux([X|Xs]).
printLine7aux([]) :- write('\\ \n'),nl.
printLine7aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine7aux(Xs).
printLine7aux([X|Xs]) :- X == 'E', printLine7aux(Xs).

printLine8([X|Xs]) :- write('   /'), printLine8aux([X|Xs]).
printLine8aux([]) :- write('\\ \n'),nl.
printLine8aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine8aux(Xs).
printLine8aux([X|Xs]) :- X == 'E', printLine8aux(Xs).

printLine9([X|Xs]) :- write('  /'), printLine9aux([X|Xs]).
printLine9aux([]) :- write('\\ \n'),nl.
printLine9aux([X|Xs]) :- X \== 'E', write(X),write(' '), printLine9aux(Xs).
printLine9aux([X|Xs]) :- X == 'E', printLine9aux(Xs).


