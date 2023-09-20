translate(Code, Meaning):-
        Code == 1 -> Meaning = 'Integer Overflow';
        Code == 2 -> Meaning = 'Division by zero';
        Code == 3 -> Meaning = 'ID Unknown'.
