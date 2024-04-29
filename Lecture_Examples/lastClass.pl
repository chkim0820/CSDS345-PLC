% Create an interpreter for a simple language: if, while, =, return
% All variables are global variables
% State of the form [[x, 5], [y, 10], [z, 3]]

% n = 10
% i = 0
% result = 1
% while (i < n)
%    i = i + 1
%    result = result * i
% return result
% 
[[n,=,10],[i,=,0],[while,[i,<,n],[i,=,[i,+,1]],[result,=,[result,*,i]]],[return,result]]

% m_state functions: m_state(construct, init_state, result_state)
m_state([while, C, B], S0, S1) :- !, m_boolean(C, S0), m_state(B, S0, SX), m_state([while, C, B], SX, S1).
m_state([while, _, _], S0, S0).

m_state([VAR, =, E], S0, S1) :- m_int(E, S0, V1), remove_binding(VAR, S0, SX), add_binding(VAR, V1, SX, S1).

m_state([[return, E], S0, S1]) :- m_int(E, S0, V), add_binding(return, V, S0, S1). % assume only return once

m_state([], S, S).
m_state([H | T], S0, S1) :- m_state(H, S0, SX), m_state(T, SX, S1).

% m_int functions
m_int([E1, +, E2], S, V) :- m_int(E1, S, V1), m_int(E2, S, V2), V is V1 + V2.
m_int([E1, *, E2], S, V) :- m_int(E1, S, V1), m_int(E2, S, V2), V is V1 * V2.
m_int(E, _, E) :- number(E). % state not needed
m_int(E, S, V) :- lookup_binding(E, S, V).

% m_boolean functions
m_boolean([E1, <, E2], S) :- m_int(E1, S, V1), m_int(E2, S, V2), V1 < V2.

% lookup_binding
lookup_binding(VAR, [[VAR, V] | _], V) :- !.
lookup_binding(VAR, [_, S], V) :- lookup_binding(VAR, S, V).

% remove_binding
remove_binding(VAR, [[VAR, _] | S], S) :- !. 
remove_binding(VAR, [X | T], [X | S]) :- remove_binding(VAR, T, S).
remove_binding(VAR, [], []).

% add_binding
add_binding(VAR, V, S, [[VAR, V] | S]).