job(technician, eleuterio).
job(technician, juvenaldo).
job(analyst, leonilde).
job(analyst, marciliano).
job(engineer, osvaldo).
job(engineer, porfirio).
job(engineer, reginaldo).
job(supervisor, sisnando).
job(chief_supervisor, gertrudes).
job(secretary, felismina).
job(director, asdrubal).
supervised_by(technician, engineer).
supervised_by(engineer, supervisor).
supervised_by(analyst, supervisor).
supervised_by(supervisor, chief_supervisor).
supervised_by(chief_supervisor, director).
supervised_by(secretary, director).


% 1
/*
   i.

   Is Sisnando responsible for an Analyst?
   
 */
/*
   ii.

   Who is responsible for supervising a technician's supervisor?
   
 */
/*
   iii.

   Who's being supervised by a Supervisor?
   
 */
/*
   iv.

   Who is Asdrúbal responsible for?
   
 */


% 2
/*
   i.

   yes
   
 */
/*
   ii.

   Y = supervisor
   
 */
/*
   iii.

   J = analyst
   P = leonilde
   
 */
/*
   iv.

   P = gertrudes
   
 */


% 3
a(X, Y):-
        job(JX, X),
        job(JY, Y),
        supervised_by(JY, JX).

b(X, Y):-
        job(JX, X),
        job(JY, Y),
        supervised_by(JX, J),
        supervised_by(JY, J).

c(X):-
        job(JX, X),
        supervised_by(J1, JX),
        supervised_by(J2, JX),
        J1 \= J2.

d(X, Y):-
        job(JX, X),
        job(JY, Y),
        supervised_by(JY, S),
        supervised_by(S, JX).
        
        