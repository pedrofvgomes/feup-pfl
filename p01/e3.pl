% pilot(Person)
pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

% team(Pilot, Team)
team(lamb, breitling).
team(besenyei, redbull).
team(chambliss, redbull).
team(maclean, mediterranean).
team(mangold, cobra).
team(jones, matador).
team(bonhomme, matador).

% pilots(Pilot, Plane)
pilots(lamb, mx2).
pilots(besenyei, edge540).
pilots(chambliss, edge540).
pilots(maclean, edge540).
pilots(mangold, edge540).
pilots(jones, edge540).
pilots(bonhomme, edge540).

% circuit(City)
circuit(istanbul).
circuit(budapest).
circuit(porto).

% won(Pilot, City)
won(jones, porto).
won(mangold, budapest).
won(mangold, istanbul).

% numberOfGates(City, Number)
numberOfGates(istanbul, 9).
numberOfGates(budapest, 6).
numberOfGates(porto, 5).

% teamWon(Team, City)
teamWon(Team, City):-
        won(Pilot, City),
        team(Pilot, Team).


% Who won the race in Porto?
i(X):-
        won(X, porto).

% ii. What team won the race in Porto?
ii(X):-
        teamWon(X, porto).

% iii. Which circuits have nine gates?
iii(X):-
        setof(City, numberOfGates(City, 9), X).

% iv. Which pilots do not fly an Edge540?
iv(X):-
        setof(Pilot, (pilots(Pilot, Plane), Plane \= edge540), X).

% v. Which pilots have won more than one circuit?
v(X):-
      setof(Pilot, (won(Pilot, City1), won(Pilot, City2), City1 \= City2), X).

% vi. What is the plane piloted by the pilot who won the race in Porto?
vi(X):-
        pilots(Pilot, X),
        won(Pilot, porto).


