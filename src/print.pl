/***************************
* OUTPUT RELATED FUNCTIONS *
***************************/

%p:- printBoard([['A'],['B','C'],['D','E','F'],['G','H','I','J'], ['K','L','M','N','O']],[['A1'],['B1','C1','D1'],['E1','F1','G1','H1','I1'], ['J1','k1','L1','M1','N1','O1','P1']]).

/*Printing an entire board 5*5*/
printBoard([HVar|[]],_):- printListV(HVar).
printBoard([HVar|Tvar], [HTri|TTri]) :- 
        printListV(HVar),
        printListT(HTri),
        printBoard(Tvar,TTri).

printListV([]):- nl. 
printListV([H|T]):- write('('),
        write(H),
        write(')'),
        write(' '),
        printListV(T). 

printListT([]):- nl. 
printListT([H|T]):- write(H),
        write(' '),
        printListT(T). 
        
        


