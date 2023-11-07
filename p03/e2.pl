list_size([], 0).
list_size([_|B], Size):-
        list_size(B, Size1),
        Size is Size1 + 1.

list_sum([A], A).
list_sum([A|B], Sum):-
        list_sum(B, Newsum),
        Sum is Newsum + A.

list_prod([A], A).
list_prod([A|B], Prod):-
        list_prod(B, Newprod),
        Prod is Newprod*A.

inner_product([A1], [A2], Result):-
        Result is A1*A2.
inner_product([A1|B1], [A2|B2], Result):-
        inner_product(B1, B2, Newresult),
        Result is Newresult + A1*A2.

count(_, [], 0).
count(Element, [A|B], N):-
        count(Element, B, N1),
        (Element =:= A -> N is N1 + 1; N is N1).        