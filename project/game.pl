:- consult('draw.pl').
:- consult('config.pl').
:- consult('utils.pl').

play:-
        draw_title,
        get_config(Gamemode, Size, Board),
        write(Gamemode),nl,
        write(Size),nl,
        write(Board).
