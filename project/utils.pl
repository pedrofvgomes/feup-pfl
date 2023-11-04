list_size([],0).
list_size([_|T], Size):-
        list_size(T, Newsize),
        Size is Newsize+1.

list_index([A|_], 0, A).
list_index([_|B], Index, Item):-
        (Index > 0 -> 
                I1 is Index-1, list_index(B,I1,Item); !).

board_index([A|B], X, Y, Item):-
        list_index([A|B], Y, Row),
        list_index(Row, X, Item).