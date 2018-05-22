% %%%% SOKOBAN SOLVER using A* algorithm
% Format of input
% # wall
% @ box
% x space for box
% o empty spot
% s sokoban


% HELPERS
% strToList(?listOfString, ?ListOfListOfAtoms).
% converts string to list of atoms
strToList([], []).
strToList([X|XS], [Cs|R]):- strToList(XS, R), string_chars(X, Cs).

% replLine(+InputL, -OutL).
% helper for finding goal x -> @ -> o
replLine([], []).
replLine([#|XS], [#|YS]):- replLine(XS, YS).
replLine([s|XS], [s|YS]):- replLine(XS, YS).
replLine([x|XS], [@|YS]):- replLine(XS, YS).
replLine([@|XS], [o|YS]):- replLine(XS, YS).
replLine([o|XS], [o|YS]):- replLine(XS, YS).
% HEURISTICS


% implementation
% SOKOBAN SPECIFIC
sokoban(X, Sol):-strToList(X,L), sokobanImpl(L, Sol).
sokobanImpl(X,Y):-findGoal(X,Y).


% findGoal(+Pos, -Goal).
% finds final position from a given one
% by appropriate replacements
findGoal([], []).
findGoal([X|XS], [Y|YS]):-replLine(X,Y), findGoal(XS, YS).

% A-STAR



test:-sokoban(["###",
               "#s#",
               "#@#",
               "#x#",
               "###"], Sol), write(Sol).
