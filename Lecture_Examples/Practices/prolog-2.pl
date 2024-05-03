% myappend: appends two lists
myappend([], L2, L2).
myappend([H|T], L2, [H|T2]) :- myappend(T, L2, T2).

% removeall: remove all occurences of an element in a list
removeall(_, [], []). % not matching anything anytime, just use a wildcard.
removeall(A, [A|T], L) :- removeall(A, T, L).
removeall(A, [H|T], [H|T2]) :- removeall(A, T, T2), A \= H.
% above might not completely remove; suggest options where the third line is executed instead
% we can put A \= H as a condition, but it will not be able to do the backward operation b/c it fails
% Order of the terms matter!!!

% contains: is an element in a list?
% never list a rule that is empty b/c true/false-based
% Don't make a separate result for returning true or false
contains(A, [A|_]). % this automatically results to true
contains(A, [_|T]) :- contains(A, T).
% No false needed b/c automatically false if no condition satisfies

% summary: order matters & T/F handled differently

%%% Practices

% replaceall: 2 atoms, list, result
replaceall(_, _, [], []).
replaceall(A, B, [A|T], [B|R]) :- replaceall(A, B, T, R).
replaceall(A, B, [H|T], [H|R]) :- replaceall(A, B, T, R), A \= H.

% insertbeforeall; 2 atoms, list, result (first atom inserted before each second atom)
insertbeforeall(_,_,[],[]).
insertbeforeall(A, B, [B|T], [A|[B|R]]) :- insertbeforeall(A, B, T, R).
insertbeforeall(A, B, [H|T], [H|R]) :- insertbeforeall(A, B, T, R), H \= B.

% flatten; takes a list w/ sublists & result without sublists
flatten([], []).
flatten([[H|T] | T2], R) :- flatten([H|T], R1), flatten(T2, R2), myappend(R1, R2, R).
flatten([[] | T], R) :- flatten(T, R).
flatten([H | T], [H | T2]) :- flatten(T, T2). 