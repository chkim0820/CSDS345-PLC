% math in Prolog
% N is M + X
% the variables on the right of the 'is' must all have values bound to them.

% sum the numbers in a list of numbers
sumnumbers([], 0).
sumnumbers([H | T], X) :- number(H), sumnumbers(T, Y), X is H + Y.
sumnumbers([_ | T], X) :- sumnumbers(T, X). 

% calculate the length of a list
mylength([], 0).
mylength([H | T], L) :- mylength(T, L2), L is L2 + 1. 

% factorial
factorial(1, 1). % forward; resolves in order; solves factorial with the next A first & completes the last stmt
factorial(N, X) :- A is N - 1, factorial(A, R1), X is R1 * N.
factorial2(1, 1). % backward; tries different values for A and B until the later stmts are satisfied; less efficient
factorial2(N, X) :- factorial2(A, B), N is A + 1, X is B * N. % search until resolving

% sumnumbersstar: sums the numbers in a list where the list can contain sublists
sumnumbersstar([], 0).
sumnumbersstar([H | T], S) :- number(H), sumnumbersstar(T, S1), S is S1 + H.
sumnumbersstar([H | T], S) :- sumnumbersstar(H, S1), sumnumbersstar(T, S2), S is S1 + S2. 
sumnumbersstar([_ | T], S) :- sumnumbersstar(T, S).
% no need to specify pattern matching for the list b/c will be rejected after function call automatically

