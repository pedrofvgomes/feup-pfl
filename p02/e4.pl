% 4a
factorial(0, 1).
factorial(N, M):-
        N > 0,
        N1 is N-1,
        factorial(N1, M1),
        M is N*M1.

% 4b
sum_rec(1,1).
sum_rec(N, Sum):-
        N > 1,
        N1 is N-1,
        sum_rec(N1, S1),
        Sum is S1 + N.