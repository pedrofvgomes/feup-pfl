:- consult('draw.pl').
:- consult('config.pl').
:- consult('utils.pl').
:- consult('game.pl').

play:-
    get_config(Board, Size),
    game_loop(Board, 'X').