:- consult('draw.pl').
:- consult('config.pl').
:- consult('utils.pl').
:- consult('game.pl').

play:-
    draw_title,
    get_config(Gamemode, Board, Size),
    write(Gamemode),nl,
    game_loop(Board, Size, Gamemode).