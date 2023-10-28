invert([], []).
invert([X], [X]).

invert([HX|TX], R):-
        invert(TX, T),
        append(T, [HX], R).