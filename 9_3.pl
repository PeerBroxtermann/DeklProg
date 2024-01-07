append([],L,L).
append([E|R],L,[E|RL]) :- append(R,L,RL).

last(L,E) :- append(_,[E],L).

member(E,L) :- append(_,[E|_],L).

delete(E,L,R) :- append(L1,[E|L2],L), append(L1,L2,R).

sublist(T,L) :- append(_,L2,L), append(T,_,L2).

lookup(K, [(K,V)|_],V).
lookup(K, [_|Rest],V) :- lookup(K,Rest,V).

member2(E,L) :- append(_,[E|Rest],L), member(E,Rest).

reverse([],[]).
reverse([X|Xs],Ys) :- reverse(Xs,Rest), append(Rest,[X],Ys).

reverse_acc(Xs,Ys) :- reverse_acc(Xs,[],Ys).
reverse_acc([],Acc,Acc).
reverse_acc([X|Xs],Acc,Ys) :- reverse_acc(Xs,[X|Acc],Ys).