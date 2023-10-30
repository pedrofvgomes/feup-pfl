list_size([],0).
list_size([_|T], Size):-
        list_size(T, Newsize),
        Size is Newsize+1.

