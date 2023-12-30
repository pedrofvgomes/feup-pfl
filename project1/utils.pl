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

find_in_board([A|B], Value, X, Y) :-
        find_in_list(A, Value, X1),
        (X1 < 0 -> (find_in_board(B,Value,X,Y1), Y is Y1 + 1); (X = X1, Y = 0)).
find_in_board([], _, -1000, -1000).

get_groups(_, [], _, _).
get_groups(Size, [Row|Rest], Player, Groups):-
    list_size([Row|Rest], S),
    Y is Size - S,
    (Y = 0 -> Groups = []; !),
    get_groups_in_row(Row, [Row|Rest], Player, Y, Groups, Newgroups),
    write('Groups updated'), nl,
    write(Newgroups), nl,
    get_groups(Size, Rest, Player, Newgroups).

get_groups_in_row([A|B], Board, Player, Y, Groups, Newgroups):-
    list_size([A|B], Size1),
    list_size(Board, Size2),
    X is Size2 - Size1,
    
    board_index(Board, X, Y, Current),
    
    (Current = Player ->(
        Y1 is Y - 1,
        Y2 is Y + 1,
        X1 is X - 1,
        X2 is X + 1,
        
        (   (find_in_board(Groups, [X, Y1], N1X, N1Y), N1X > -1, N1Y > -1) -> (NX is N1X, NY is N1Y)
        ;   (   (find_in_board(Groups, [X, Y2], N2X, N2Y), N2X > -1, N2Y > -1) -> (NX is N2X, NY is N2Y)
        ;   (   (find_in_board(Groups, [X1, Y], N3X, N3Y), N3X > -1, N3Y > -1) -> (NX is N3X, NY is N3Y)
        ;   (   (find_in_board(Groups, [X2, Y], N4X, N4Y), N4X > -1, N4Y > -1) -> (NX is N4X, NY is N4Y)
        ;   (NX is -1, NY is -1)))), 
        
        (NX > -1, NY > -1) ->
            % Found a neighbor in an existing group
            (list_index(Groups, NY, Group),
             Newgroup = [[X, Y] | Group],
             update_list(Groups, NY, Newgroup, Newgroups),
             write('Added'), nl,
             write(Newgroup), nl,
             write(Newgroups)
            )
        ;
        Newgroups = [[[X, Y]] | Groups])
    ); !),
    get_groups_in_row(B, Board, Player, Y, Newgroups, _).
