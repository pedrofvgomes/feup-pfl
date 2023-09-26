a(a1, 1).
a(a2, 2).
a(a3, n).
b(1, b1).
b(2, b2).
b(n, b3).

c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).

/*
   ex 3c

   i)
        a(A, 2).
        A = a2.

   ii)
        b(A, foobar).
        no

   iii)
        c(A, b3).
        A = a3

   iv)
        c(A, B).
        A = a1, B = b1

   v)
        d(A, B)
        no
 */