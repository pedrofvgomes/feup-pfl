pairs(X, Y) :- d(X), q(Y).
pairs(X, X) :- u(X).

u(1).
d(2).
d(4).
q(4).
q(16).

% ex 2a
% SEARCH TREE FOR THE QUERY: pairs(X,Y).

/*
                [pairs(X,Y)]
                      |                               
                      |                               
                      |                               
                      |                               
                      |
                 [d(X), q(Y)]
                      /\              
                     /  \            
             X = 2  /    \          
                   /      \        
                  /       ...
                 /
             [q(Y)]
               /  
              /  
      Y = 4  /  
            /
           /
         []
 */