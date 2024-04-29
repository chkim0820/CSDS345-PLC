% removeall
removeall(X, [], []).
removeall(X, [X | T], R) :- !, removeall(X, T, R). % ! locks X the first value
removeall(X, [H | T], [H | R]) :- removeall(X, T, R). % X \= H

% use cut with contains function
contains(X, [X|_]) :- !.
contains(X, [_|T]) :- contains(X, T).

% challenge: use cut with factorial
% Forward version
factorial(0, 1) :- !.
factorial(N, R) :- M is N - 1, factorial(M, S), R is N * S. % end once factorial found
% factorial(N, R) :- M is N - 1, factorial(M, S), !, R is N * S. ; This would cut every iteration
factorial2(0, 1).
factorial2(N, R) :- factorial2(A, B), N is A + 1, R is B * N.
