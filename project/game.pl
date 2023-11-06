game_loop(Board, Gamemode, Player):-
        draw_board(Board),
        write(Gamemode),
%        check_winner(Board),
        nl,nl,
        get_move(Board, Player, Updatedboard),
        (Player = 'X' -> Newplayer = 'O'; Newplayer = 'X'),
        write('a'),
        game_loop(Updatedboard, Gamemode, Newplayer).
/*
check_winner(Board):-
        get_groups(Board, 'X', Groups1),
        list_size(Groups1, S1),
        get_groups(Board, 'O', Groups2),
        list_size(Groups2, S2),

        (S1 = 1 -> win_screen('X'); 
        (S2 = 1 -> win_screen('O'); !)).
*/

get_move(Board, Player, Updatedboard) :-
        write('================'), nl,
        write('    Player '),
        write(Player), nl,
        write('================'), nl, nl,

        write('Which piece would you like to move? (e.g., A1) '), nl,
        read_line([A|B]),
        Column1 is A - 65,
        [A1|B1] = B,
        Row1 is A1 - 49,
        nl,

        write('Where would you like to move it to? (e.g., A1)'), nl,
        read_line([A2|B2]),
        Column2 is A2 - 65,
        [A3|B3] = B2,
        Row2 is A3 - 49,
        nl,

        board_index(Board, Column1, Row1, Value1),
        board_index(Board, Column2, Row2, Value2),

        list_size(Board, Size),
        ((Value1 = Player, Value2 = ' ', Row1 > -1, Row1 < Size, Row2 > -1, Row2 < Size, Column1 > -1, Column1 < Size, Column2 > -1, Column2 < Size) ->
                (update_board(Board, Column2, Row2, Value1, Tempboard),
                update_board(Tempboard, Column1, Row1, Value2, Updatedboard),
                nl, nl)
        ;
        (nl,
        write('Invalid move - the chosen piece should be yours and you should choose an empty space to move it to! Try again'), nl, nl, nl,
        get_move(Board, Player, Updatedboard))
        ).
