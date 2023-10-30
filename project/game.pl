game_loop(Board, Size, Gamemode):-
        format('Game starting ~w ~w ~w', [Board, Size, Gamemode]),nl,
        draw_board(Board, Size),
        nl.