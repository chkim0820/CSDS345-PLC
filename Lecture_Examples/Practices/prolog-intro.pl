% this is a comment
% A prolog file is basically a database
% logical language; infer from facts in the database
% Variables start with an upper case letter; everything else with lowercase.
% Database facts end in a period.


% Creating a database
parentof(arthur, ron).
praentof(molly, ron).
praentof(arthur, ginny).
parentof(molly, ginny).
parentof(james, harry).
parentof(lily, harry).
parentof(ginny, jamesjr).
parentof (harry, jamesjr).

married(harry, ginny).
married(lily, james).
married(molly, arthur).

% false just means it's not in the database
% no means cannot verify from the database
% parentof(X, harry), where X is a variable, suggests different possible values for X. 
% hitting 'enter' after accepts the suggested answer, and ';' searches through the database for more possible answers

% A rule is of the form A :- B,C,D. This means B "AND" C "AND" D implies A
grandparent(X, Y) :- parentof(X, A), parentof(A, Y).

% Inputting variables in method calls would return possible values for those variables!

% another rule for siblings
sibling(A, B) :- parentof(X, A), parentof(X, B), A \= B.

cousin(A, B) :- parentof(X, A), parentof(Y, B), sibling(X, Y), X \= Y, A \= B.

parentinlawof(A, B) :- parentof(A, X), married(X, B).


% Prolog lists

% Prolog's myappend will be of form
% myappend(input1, input2, output)
myappend([], L2, L2).
myappend([H|T], L2, [H|S]) :- myappend(T, L2, S).

% could find different possible values for the input variables
myreverse([], []).
myreverse([H|T], X) :- myreverse(T, R), myappend(R, [H], X).