% Maze

/*
connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).
*/

connected(1,2).
connected(2,1).
connected(1,3).
connected(3,4).


path2(Start, End, _) :- connected(Start, End).
path2(Start, End, L) :- append(L, [Start], NewL), connected(Start, X), \+ member(X, L), path2(X, End, NewL).

path(Start, End) :- path2(Start, End, []).
