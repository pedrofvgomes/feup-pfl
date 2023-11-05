get_config(Gamemode, Board, Size):-
        write('\33\[2J'),nl,
        write('SELECT GAME MODE'),nl,
        write('1) Player VS Player'),nl,
        write('2) Player VS AI'),nl,
        write('3) AI VS AI'),nl,
        read_line([G1|_]),
        char_code(Gamemode, G1),
        
        write('\33\[2J'),nl,
        write('SELECT BOARD SIZE'),nl,
        write('1) 6x6'),nl,
        write('2) 8x8'),nl,
        write('3) 10x10'),nl,
        read_line([O1|_]),
        Option is O1-48,
        Size is Option*2 + 4,

        load_board(Size, Board).

load_board(6, Board):-
    Board = [[' ','X','O','X','O',' '],
             ['X','O','X','O','X','O'],
             ['O','X','O','X','O','X'],
             ['X','O','X','O','X','O'],
             ['O','X','O','X','O','X'],
             [' ','O','X','O','X',' ']].

load_board(8, Board):-
    Board = [[' ','X','O','X','O','X','O',' '],
             ['X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X'],
             ['X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X'],
             ['X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X'],
             [' ','O','X','O','X','O','X',' ']].

load_board(10, Board):-
    Board = [[' ','X','O','X','O','X','O','X','O',' '],
             ['X','O','X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X','O','X'],
             ['X','O','X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X','O','X'],
             ['X','O','X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X','O','X'],
             ['X','O','X','O','X','O','X','O','X','O'],
             ['O','X','O','X','O','X','O','X','O','X'],
             [' ','O','X','O','X','O','X','O','X',' ']].
