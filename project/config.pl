get_config(Gamemode, Board, Size):-
        write('\33\[2J'),nl,
        write('SELECT GAME MODE'),nl,
        write('1) Player VS Player'),nl,
        write('2) Player VS AI'),nl,
        write('3) AI VS AI'),nl,
        read(Gamemode),
        
        write('\33\[2J'),nl,
        write('SELECT BOARD SIZE'),nl,
        write('1) 6x6'),nl,
        write('2) 8x8'),nl,
        write('3) 10x10'),nl,
        read(Option),
        Size is Option*2 + 4,

        load_board(Size, Board).

load_board(6, Board):-
    Board = [['N',0,1,0,1,'N'],
             [0,1,0,1,0,1],
             [1,0,1,0,1,0],
             [0,1,0,1,0,1],
             [1,0,1,0,1,0],
             ['N',1,0,1,0,'N']].

load_board(8, Board):-
    Board = [['N',0,1,0,1,0,1,'N'],
             [0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0],
             [0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0],
             [0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0],
             ['N',1,0,1,0,1,0,'N']].

load_board(10, Board):-
    Board = [['N',0,1,0,1,0,1,0,1,'N'],
             [0,1,0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0,1,0],
             [0,1,0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0,1,0],
             [0,1,0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0,1,0],
             [0,1,0,1,0,1,0,1,0,1],
             [1,0,1,0,1,0,1,0,1,0],
             ['N',1,0,1,0,1,0,1,0,'N']].
