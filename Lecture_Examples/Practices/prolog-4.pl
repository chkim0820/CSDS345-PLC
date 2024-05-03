% efficient version of factorial
factorial(0, 1).
factorial(N, X) :- M is N - 1, factorial(M, Y), X is N * Y.

% less efficient but more flexible version
factorial2(0, 1).
factorial2(N, X) :- factorial2(M, Y), N is M + 1, X is Y * N.

% The prolog "cut"
% The cut is a predicate that is always true
% When the search crosses the cut, any variables that are bound to values have those bindings fixed.
% All recursing and searching will only be on the rules it encounters after the cut.

removeall(_, [], []).
removeall(A, [A|T], R) :- removeall(A, T, R), !. % prevents from going to H
removeall(A, [H|T], [H|R]) :- removeall(A, T, R).