% %%%% SOKOBAN SOLVER using A* algorithm
% Format of input
% # wall
% @ box
% * placed box
% x space for box
% o empty spot
% s sokoban


% HELPERS
% strToList(?listOfString, ?ListOfListOfAtoms).
% converts string to list of atoms
strToList([], []).
strToList([X|XS], [Cs|R]):- strToList(XS, R), string_chars(X, Cs).

%extract(Ss, Y)
% unifies Y with all elements from Ss
extract([X|_], X).
extract([_|Ss], Y):-extract(Ss, Y).

% replLine(+InputL, -OutL).
% helper for finding goal x -> @ -> o
replLine([], []).
replLine([#|XS], [#|YS]):- replLine(XS, YS).
replLine([s|XS], [s|YS]):- replLine(XS, YS).
replLine([x|XS], [*|YS]):- replLine(XS, YS).
replLine([@|XS], [o|YS]):- replLine(XS, YS).
replLine([o|XS], [o|YS]):- replLine(XS, YS).

% cutFirstColumn(+LL, -LLO, -RL)
% takes list of lists and from each list erases head
% RL is the cut part
cutFirstColumn([], [], []).
cutFirstColumn([[X|XS]|YS], [XS|YSCut], [X|Rem]):-cutFirstColumn(YS, YSCut, Rem).

% addFirstColumn(+LL, +Col, -R)
% add Column to the beginning of LL and return it in R
addFirstColumn([], [], []).
addFirstColumn([Line|LL], [XS|Col], [Line2|Res]):-
	Line2=[XS|Line], addFirstColumn(LL, Col, Res).



% HEURISTICS
% TODO
heur(_,_,R):-R is 0.

% implementation
% SOKOBAN SPECIFIC
% findGoal(+Pos, -Goal).
% finds final position from a given one
% by appropriate replacements
findGoal([], []).
findGoal([X|XS], [Y|YS]):-replLine(X,Y), findGoal(XS, YS).

% nextMove(+F, -T, -D)
% get all possible moves from F with the direction D (lrud)
nextMove(F,T,D):-setof([M,D], nextMoveImpl(F,M,D), MS), extract(MS, [T,D]).

% nextMoveImplt(F, T, D)
% implementation of nextMove
% it cuts columns and rows in all possible ways until we
% find sokoban and then try to move him.
% remove first column, run recursively and give it back
nextMoveImpl(X, M, D):-X\=[], cutFirstColumn(X,Y, Col),
	nextMoveImpl(Y, Y2, D), addFirstColumn(Y2, Col, M).
% cut first line
nextMoveImpl([X|Y], [X|M], D):-X\=[], nextMoveImpl(Y, M, D).
% left
nextMoveImpl([[o,s|X]|Y], [[s,o|X]|Y], l).
nextMoveImpl([[x,@,s|X]|Y], [[*,s,o|X]|Y], l).
nextMoveImpl([[o,@,s|X]|Y], [[@,s,o|X]|Y], l).

% right
nextMoveImpl([[s,o|X]|Y], [[o,s|X]|Y], r).
nextMoveImpl([[s,@,x|X]|Y], [[o,s,*|X]|Y], r).
nextMoveImpl([[s,@,o|X]|Y], [[o,s,@|X]|Y], r).

% down
nextMoveImpl([[s|X],[o|Y]|R], [[o|X],[s|Y]|R], d).
nextMoveImpl([[s|X],[@|Y],[x|Z]|R], [[o|X],[s|Y],[*|Z]|R], d).
nextMoveImpl([[s|X],[@|Y],[o|Z]|R], [[o|X],[s|Y],[@|Z]|R], d).

% up
nextMoveImpl([[o|X],[s|Y]|R], [[s|X],[o|Y]|R], u).
nextMoveImpl([[x|X],[@|Y],[s|Z]|R], [[*|X],[s|Y],[o|Z]|R], u).
nextMoveImpl([[o|X],[@|Y],[s|Z]|R], [[@|X],[s|Y],[o|Z]|R], u).



% A-STAR
sokoban(X, Sol):-strToList(X,Start), findGoal(Start, Goal),
	sokobanIDA(Start, Goal, 0, Sol).

sokobanIDA(Start, Goal, Max, Moves):-
	sokobanSearch(Start, Goal, 0, Max, Moves),!.
sokobanIDA(Start, Goal, Max, Moves):-
	Max < 10, Max2 is Max +1, % TODO constant
	sokobanIDA(Start, Goal, Max2, Moves).


% search Goal from Start which is at most Max moves away
sokobanSearch(Goal, Goal, _, _, []):-!.
sokobanSearch(Start, Goal, Depth, Max, [Dir|Moves]):-
	nextMove(Start, Move, Dir),
	heur(Start, Goal, R),
	Depth2 is Depth + 1 + R,
	(Depth2 < Max; Depth2 is Max),
	sokobanSearch(Move, Goal, Depth2, Max, Moves).




% TESTING
test:-sokoban(["###",
               "#s#",
               "#@#",
               "#x#",
               "###"], Sol), write(Sol).

test2:-sokoban(["oo###oo",
                "oo#x#oo",
                "###@###",
                "#x@s@x#",
                "###@###",
                "oo#x#oo",
                "oo###oo"], Sol), write(Sol).

test3:-sokoban(["#######",
               "#xxxo###",
               "#oxo@oo#",
               "#oo@@@o#",
               "####oos#",
               "########"], Sol), write(Sol).

% test:-strToList(["###",
               % "#s#",
               % "#@#",
               % "#x#",
               % "###"], Sol), nextMove(Sol, X,Y), write(X).
