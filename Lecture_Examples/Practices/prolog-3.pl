% Numbers in Prolog
% X is A + B
% Not the same as searching for database; calculates actual numbers
% This calculates x by adding A and B. A and B must both have values at the time we do the calculation.

% sum al lnumbers in a list of numbers
sumlist([], 0).
sumlist([H|T], X) :- sumlist(T, Y), X is Y + H.
% cannot do backwards here since we don't know H
% Has to calculate the number

% returns the length of a list
len([],0).
len([_|T], N) :- len(T, M), N is M + 1.
% backward motion works here b/c H didn't need to be instantiated first
% Everything left to 'is' should have a value already

%% Practices

% factorial
factorial(0,1).
factorial(N, R) :- M is N - 1, factorial(M, R1), R is R1 * N.

