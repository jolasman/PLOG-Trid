
/*board generation */
generateBoard(X,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1) :- X = [ 
                             ['(',A,')'],
                                ['  ',A1], 
                         ['(',B,')', '(',C,')'],
                       [' ',B1,'\\', C1, '/',D1],
                     ['(',D,')','(',E,')','(',F,')'],
              [' ',E1,'\\', F1, '/',G1,'\\', H1, '/',I1],
               ['(',G,')','(',H,')','(',I,')','(',J,')'],
         [' ',J1,'\\', K1, '/',L1,'\\', M1, '/',N1,'\\', O1],
            ['(',K,')','(',L,')','(',M,')','(',N,')', '(',O,')']    ].



/*board generation */
generateEmptyBoard(X) :- X = [ 
                             ['(','A',')'],
                                ['A1'], 
                         ['(','B',')', '(','C',')'],
                       ['  ','B1','\\', 'C1', '/','D1'],
                     ['(','D',')','(','E',')','(','F',')'],
              ['  ','E1','\\', 'F1', '/','G1','\\', 'H1', '/','I1'],
               ['(','G',')','(','H',')','(','I',')','(','J',')'],
         ['  ','J1','\\', 'K1', '/','L1','\\', 'M1', '/','N1','\\', 'O1'],
            ['(','K',')','(','L',')','(','M',')','(','N',')', '(','O',')']    ].


/*********************************** verifica��o das jogadas  ***************************/



/********************************mudar pe�a no tabuleiro**************************************/


%Para efectuar a jogada que o jogador indicou, ap�s verifica��o da mesma
setPieceAt(Board1, Xpos, Ypos, Board2, Piece) :- changePiece(Board1, 1, Xpos, Ypos, Piece, Board2).

%recebe o tabuleiro de jogo e isola a coluna pretendida.
changePiece([B|Bs], N, X, Y, Piece, Board2) :-
        N == Y,
        changeLinePiece(B, 1, X, Piece, BoardAux),% chama o changeline com a cabe�a da lista que e' a coluna selected
        append([BoardAux], Bs, Board2). % board 2 e' Bs (colunas para a frente) com a cabe�a vazia.

changePiece([B|Bs], N, X, Y, Piece, Board2) :-
        N < Y,
        N2 is N + 1,
        changePiece(Bs, N2, X, Y, Piece, BoardAux), %chamo com as restantes listas da lista (colunas) com o aux vazio
        append([B], BoardAux, Board2). % guardo a lista nao alterada em Board2.

%percorre a linha e coloca a pe�a na posicao X tendo em conta a coluna (lista) escolhida em change piece
changeLinePiece([_|Ls], N, X, Piece, L2) :-
        N == X,
        append([Piece], Ls, L2). %coloca a pe�a na cabe�a da lista, que corresponde � posi��o X pretendida

changeLinePiece([L|Ls], N, X, Piece, L2) :-
        N < X,
        N2 is N + 1,
        changeLinePiece(Ls, N2, X, Piece, Laux),%chama com os restantes elementos da linha
        append([L], Laux, L2). %guarda o elemento da posi��o n em L2. (guarda os que nao sao alterados)

/**************************************************************************************************/

/************************************saber pe�a no tabuleiro *************************************/
%Para saber qual a pe�a que est� numa determinada coordenada 
%codigo de outra pessoa. adaptar estes predicados. contudo parece que isto funciona assim
returnPieceAt(Board, X, Y, Piece) :- boardLine(Board, 1, Y, Line), % come�a no 1 por causa da linha dos numeros
        linePiece(Line, 1, X, Piece). %tem de ser com 1 para dar certo na linha. devido ao changelinepice ser 1 tambem
       
%recebe o tabuleira e isola a lista que � referente � coluna do tabuleiro
boardLine([B|_], N, Y, Line) :-
        N == Y,
        append([], B, Line). % se for a cabe�a da lista de listas (primeira lista) guarda a lista em line
%seleciona a lista que queremos basicamente

boardLine([_|Bs], N, Y, Line) :-
        N < Y,
        N2 is N + 1,
        boardLine(Bs, N2, Y, Line). %percorre a lista de listas ate que a lista que queremos esteja � cabe�a

%recebe a linha ja escolhida em boardline e retorna a pe�a que est� na posi��o X.
linePiece([L|_], N, X, Piece) :-
        N == X,
        Piece = L. %se a posi��o x for a cabe�a da lista a pe�a e' a cabe�a da lista

linePiece([_|Ls], N, X, Piece) :-
        N < X,
        N2 is N + 1,
        linePiece(Ls, N2, X, Piece). %percorre a lista at� que a posi��o X seja a cabe�a da lista

/*******************************************************************************************************/



