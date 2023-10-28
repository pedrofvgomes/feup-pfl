draw_title:-
    write('title'),nl.

draw_board([]).
draw_board([Line | Lines]):-
    draw_line(Line),
    nl,
    draw_board(Lines).
    
draw_line([]).
draw_line([A|B]) :-
  write(A),
  write(' '),
  draw_line(B).
