=(X, X).

ehemann(christine, heinz).
ehemann(maria, fritz).
ehemann(monika, herbert).
ehemann(angelika, hubert).

mutter(herbert, christine).
mutter(angelika, christine).
mutter(hubert, maria).
mutter(susanne, monika).
mutter(norbert, monika).
mutter(andreas, angelika).

vater(K, V) :- ehemann(M, V), mutter(K, M).

elter(K, E) :- vater(K, E).
elter(K, E) :- mutter(K, E).

grossvater(E, G) :- elter(E, F), vater(F, G).

grossvaeter(Gs) :- findall([E, G], grossvater(E, G), Gs).

vorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).
vorfahre(N, V) :- elter(N, V).

geschwister(S, P) :- mutter(S, M), mutter(P,M), \+(=(P, S)).

% Test query: "vorfahre(X,Y).".
% Expected result (with dfs): No solutions and non-termination.
% Expected result (with bfs or iddfs): Multiple solutions, but still
% non-termination.
% Tests completeness of bfs and iddfs.

% Test query: "geschwister(X,Y).".
% Expected result: 4 solutions, "{X -> herbert, Y -> angelika}",
% "{X -> angelika, Y -> herbert}", "{X -> susanne, Y -> norbert}", and
% "{X -> norbert, Y -> susanne}".
% Tests implementation of negation as a failure.
% BONUS: Negation

% Test query: "grossvaeter(Xs).".
% Expected result: One solution, "{Xs -> [[susanne, heinz], [norbert, heinz],
% [andreas, fritz], [andreas, heinz]]}".
% Tests implementation of encapsulation.
% BONUS: Encapsulation
