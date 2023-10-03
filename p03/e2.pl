% ex 2a

list_size([],0).
list_size([_|T], Size):-
        list_size(T, Newsize),
        Size is Newsize+1.

% ex 2b
list_sum([], 0).
list_sum([H|T], Sum):-
        list_sum(T, Newsum),
        Sum is Newsum+H.

% ex 2c
list_prod([X], X).
list_prod([H|T], Prod):-
        list_prod(T, Newprod),
        Prod is Newprod*H.

% ex 2d
inner_product([X], [Y], [X*Y]).
inner_product([H1|T1], [H2|T2], Prod):-
        inner_product(T1, T2, Newprod),
        Prod is Newprod + H1*H2.

% ex 2e
count(_, [], 0).
count(X, [H|T], N):-
        count(X, T, N1),
        if(X==H, N is N1+1, N is N1).
