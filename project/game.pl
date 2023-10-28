:- consult('draw.pl').
:- consult('config.pl').
:- consult('utils.pl').

play:-
        draw_title,
        get_config(Gamemode, Board),
        write(Gamemode),nl,
        draw_board(Board).