% This is a comment
%
% We have facts, rules, and queries
% A fact ends in a period.
% A rule is   A :- B, C, D.
%   Which is interpreted as "B and C and D implies A"
%
% Variables start with a capital letter.

% A prolog program is a database of facts.

% Facts about some famous families.
parentof(harry, james).
parentof(dudley, vernon).
parentof(dudley, petunia).
parentof(harry, lily).
parentof(ron, molly).
parentof(ron, arthur).
parentof(precy, molly).
parentof(fred, molly).
parentof(fred, arthur).
parentof(george, molly).
parentof(george, arthur).
parentof(severus, harry).
parentof(severus, ginny).
parentof(jamesjr, harry).
parentof(jamesjr, ginny).
parentof(ginny, arthur).
parentof(ginny, ron).

married(ron, hermione).
married(ginny, harry).
married(molly, arthur).
married(petunia, vernon).

parentof(petunia, joy).
parentof(lily, joy).

% A rule is A :- B, C, D.
% Which means B and C and D implies A
grandparentof(A, B) :- parentof(A, C), parentof(C, B)

% siblings
siblings(A, B) :- parentof(A, C), parentof(B, C), A \= B.
% uncleauntof
auntuncleof(A, B) :- parentof(A, C), siblings(C, B).
% cousins

% Prolog and lists are recursive functions
% (define myappend)
%    (lambda (l1 l2)
%       (if (null? l1)
%           l2
%           (cons (car l1) (myappend (cdr l1) l2))))
%
% myappend(list1, list2, result).
myappend([], L2, L2).
myappend([H | T], L2, [H | R]) :- myappend(T, L2, R). % everything by headers! same letters mean the same thing

% Use myappend to create myreverse
myreverse([], []).
%myreverse([H | T], R) :- myappend(myreverse(T, R), [H], []).
myreverse([H | T], R) :- myreverse(T, R2), myappend(R2, [H], R). % Don't do it the opposite order; runs forever