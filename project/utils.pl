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

update_list([], _, _, []).
update_list([_|B], 0, Value, [Value|B]).
update_list([A|B1], Index, Value, [A|B2]) :-
        Index > 0,
        I1 is Index - 1,
        update_list(B1, I1, Value, B2).

update_board([A1|B1], X, Y, Value, [A2|B2]) :-
        list_index([A1|B1], Y, Row),   
        update_list(Row, X, Value, NewRow),
        update_list([A1|B1], Y, NewRow, [A2|B2]). 

find_in_list([A|_], A, 0).
find_in_list([], _, -100000).
find_in_list([_|B], Value, Index):-
        find_in_list(B, Value, Index1),
        Index is Index1 + 1.

find_in_board([Row|Rest], Value, X, Y) :-
        find_in_list(Row, Value, X1),
        (X1 < 0 -> (find_in_board(Rest,Value,X,Y1), Y is Y1 + 1); (X = X1, Y = 0)).
find_in_board([], _, -1000, -1000).
            
