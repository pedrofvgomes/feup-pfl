:- consult('draw.pl').
:- consult('config.pl').
:- consult('utils.pl').

play:-
        draw_title,
        get_config(Gamemode, Board, Size),
        write(Gamemode),nl,
        draw_board(Board, Size).