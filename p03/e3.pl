invert([A], [A]).
invert([A|B], Inverted):-
        invert(B, Newinverted),
        append(Newinverted, [A], Inverted).

del_one(_, [], []).
del_one(A, [A|B], B).
del_one(Element, [A|B], [A|R]):-
        del_one(Element, B, R).

del_all(_, [], []) :- !.
del_all(X, [X|Xs], Y) :- !, del_all(X, Xs, Y).
del_all(X, [T|Xs], Y) :- !, del_all(X, Xs, Y2), append([T], Y2, Y).

del_all_list([], List, List).
del_all_list([Element|RestElements], List, Return) :-
    del_all(Element, List, NewList), 
    del_all_list(RestElements, NewList, Return).

del_dups([], []).
del_dups([A|B], L):-
        member(A,B), !,
        del_dups(B, L).
del_dups([A|B1], [A|B2]):-
        del_dups(B1,B2).

replicate(0, _, []).
replicate(Amount, Element, List):-
        replicate(Newamount, Element, Newlist),
        append([Element], Newlist, List),
        Amount is Newamount + 1.

insert_elem(0, List1, Element, [Element|List1]).
insert_elem(Index, [A1|B1], Element, List):-
        insert_elem(Newindex, B1, Element, Newlist),
        Index is Newindex + 1,
        append([A1], Newlist, List).

delete_elem(0, [Element|List1], Element, List1).
delete_elem(Index, [A1|B1], Element, List):-
        delete_elem(Newindex, B1, Element, Newlist),
        Index is Newindex + 1,
        append([A1], Newlist, List).

replace(0, [Old|List1], Old, New, [New|List1]).
replace(Index, [A1|B1], Old, New, List):-
        replace(Newindex, B1, Old, New, Newlist),
        Index is Newindex + 1,
        append([A1], Newlist, List).