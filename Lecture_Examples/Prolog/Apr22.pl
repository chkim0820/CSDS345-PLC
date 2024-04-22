% recursive functions with lists
myappend([], L, L).
myappend([H|T], L, [H|T2]) :- myappend(T, L, T2).

% removeall
removeall(_, [], []). % use wild card instead of specifying "A"
removeall(A, [A|T], R) :- removeall(A, T, R). % the first in the list is equal to "A"
removeall(A, [H|T], [H|R]) :- removeall(A, T, R), A \= H. % append the head since it's not removing; recurse before checking

% contains: is X contained in list L?
%    ((null? lis) #f)
%    ((eq? x (car lis)) #t)
%    (else (contains? x (cdr lis)))
% returning boolean; don't include (null? lis) b/c Prolog bool works with the database
contains(X, [X|_]). % variables that exist only once => use wildcard
contains(X, [H|T]) :- contains(X, T).

% replaceall
% replaceall(A, B, [A, B], R) => [B,B]
replaceall(_, _, [], []). 
replaceall(A, B, [A|T], [B|R]) :- replaceall(A, B, T, R).
replaceall(A, B, [H|T], [H|R]) :- replaceall(A, B, T, R), A \= H.

% insertbeforeall(x, a, [a,b,a,c], R) => R = [x,a,b,x,a,c]
insertbeforeall(_, _, [], []).
insertbeforeall(X, A, [A | T], [X, A | R]) :- insertbeforeall(X, A, T, R). % just list all atoms being added
insertbeforeall(X, A, [H | T], [H | R]) :- insertbeforeall(X, A, T, R).

% flatten([[a,b],[[c]]], R) => R = [a,b,c]
flatten([], []).
flatten([[H|T] | T2], R) :- flatten([H|T], R1), flatten(T2, R2), myappend(R1, R2, R).
flatten([[] | T], R) :- flatten(T, R).
flatten([H | T], [H | T2]) :- flatten(T, T2). 

% containsstar: is X in a list that can contain sublists?
