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