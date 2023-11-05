game_loop(Board, Size, Gamemode):-
        draw_board(Board, Size),
        write(Gamemode),
        check_winner(Board),
        % get_move(Board),
        nl.

check_winner(Board):-
        nl,
        write('no winner'),
        nl.


