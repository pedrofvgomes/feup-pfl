draw_title:-
    write('title'),nl.

draw_board([A|B]):-
    write('\33\[2J'),nl,
    list_size(A, Size),
    draw_board([A|B],Size).
draw_board([], Size):-
    Tempsize is Size,
    write('  '),
    draw_division(Tempsize).
draw_board([Line | Lines], Size):-
    (list_size([Line|Lines],Size) -> draw_x_axis(Size); write('')),
    Tempsize1 is Size,
    write('  '),
    draw_division(Tempsize1),
    nl,
    list_size([Line | Lines], X),
    Y is Size - X + 1,
    (Y < 10 -> format('~w |', Y); format('~w|', Y)),
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

draw_x_axis(6):-
    write('    A   B   C   D   E   F'),nl.
draw_x_axis(8):-
    write('    A   B   C   D   E   F   G   H'),nl.
draw_x_axis(10):-
    write('    A   B   C   D   E   F   G   H   I   J'),nl.


win_screen(Winner):-
    write('\33\[2J'),nl,
    format('+-----------------+\n'),
    format('|                 |\n'),
    format('| Player ~w wins! |\n',Winner),
    format('|                 |\n'),
    format('+-----------------+\n').