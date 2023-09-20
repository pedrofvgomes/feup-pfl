% male(Person)
male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(bo).
male(dylan).
male(luke).
male(rexford).
male(calhoun).
male(george).

% female(Person)
female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(pameron).
female(haley).
female(alex).
female(lily).
female(poppy).

% parent(Parent, Child)
parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(dede, mitchell).
parent(jay, claire).
parent(jay, mitchell).
parent(jay, joe).
parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).
parent(barb, cameron).
parent(barb, pameron).
parent(merle, cameron).
parent(merle, pameron).
parent(phil, haley).
parent(phil, alex).
parent(phil, luke).
parent(claire, haley).
parent(claire, alex).
parent(claire, luke).
parent(mitchell, lily).
parent(mitchell, rexford).
parent(cameron, lily).
parent(cameron, rexford).
parent(pameron, calhoun).
parent(bo, calhoun).
parent(dylan, george).
parent(dylan, poppy).
parent(haley, george).
parent(haley, poppy).

% father(Father, Child)
father(Father, Child) :-
        parent(Father, Child),
        male(Father).

% mother(Mother, Child)
mother(Mother, Child) :- parent(Mother, Child), female(Mother).

% grandparent(Grandparent, Grandchild)
grandparent(Grandparent, Grandchild) :- parent(Grandparent, X), parent(X, Grandchild).

% grandfather(Grandfather, Grandchild)
grandfather(Grandfather, Grandchild) :- grandparent(Grandfather, Grandchild), male(Grandfather).

% grandmother(Grandmother, Grandchild)
grandmother(Grandmother, Grandchild) :- grandparent(Grandmother, Grandchild), female(Grandmother).

% siblings(Sibling1, Sibling2)
siblings(Sibling1, Sibling2) :- mother(Mother, Sibling1), mother(Mother, Sibling2), father(Father, Sibling1), father(Father, Sibling2), Sibling1 \== Sibling2.

% halfSiblings(HalfSibling1, HalfSibling2)
halfSiblings(HalfSibling1, HalfSibling2) :-
        parent(A, HalfSibling1),
        parent(B, HalfSibling1),
        parent(B, HalfSibling2),
        parent(C, HalfSibling2),
        A \== B,
        B \== C,
        C \== A,
        HalfSibling1 \== HalfSibling2.

% cousins(Cousin1, Cousin2)
cousins(Cousin1, Cousin2) :-
        parent(Parent1, Cousin1),
        parent(Parent2, Cousin2),
        siblings(Parent1, Parent2).

% uncle(Uncle, Nibling)
% irmao de um dos pais
% pai de um dos primos
uncle(Uncle, Nibling) :-
        parent(Parent, Nibling),
        siblings(Parent, Uncle),
        male(Uncle);
        cousins(Nibling, Cousin),
        parent(Uncle, Cousin),
        male(Uncle).

% aunt(Aunt, Nibling)
% irmã de um dos pais
% mãe de um dos primos
aunt(Aunt, Nibling) :-
        parent(Parent, Nibling),
        siblings(Parent, Aunt),
        female(Aunt);
        cousins(Nibling, Cousin),
        mother(Aunt, Cousin).

% Marriage predicate
marriage(Person1, Person2, Year) :-
    married(Person1, Person2, Year) ; married(Person2, Person1, Year).

% Marriage facts
married(jay, gloria, 2008).
married(jay, dede, 1968).

% Divorce predicate
divorce(Person1, Person2, Year) :-
    married(Person1, Person2, MarriageYear),
    Year > MarriageYear.

% Divorce facts
divorce(jay, dede, 2003).