draw_title:-
    write('title'),nl.

draw_board([], Size):-
    Tempsize is Size,
    write('  '),
    draw_division(Tempsize).
draw_board([Line | Lines], Size):-
    Tempsize1 is Size,
    write('  '),
    draw_division(Tempsize1),
    nl,
    list_size([Line | Lines], X),
    Y is Size - X + 1,
    format('~w |', Y),
    draw_line(Line),
    write('|'),
    nl,
    draw_board(Lines, Size).


draw_line([]).
draw_line([A|B]) :-
    format(' ~w ', A),
    (list_size(B, 0) -> write(''); write('|')),
    draw_line(B).

draw_division(0) :- !.
draw_division(Size) :-
    Size > 0,
    write('+---'),
    (Size =:= 1 -> write('+'); write('')),
    NewSize is Size - 1,
    draw_division(NewSize).
