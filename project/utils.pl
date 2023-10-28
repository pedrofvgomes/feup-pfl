switch(X, [Value:Goal|Cases]):-
        X=Value -> call(Goal);
        switch(X, Cases).