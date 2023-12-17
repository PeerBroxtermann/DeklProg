and(true,true,true).
and(false,_,false).
and(_,false,false).

% :- ! sorgt dafuer, dass nach einer gefunden Regel die Suche nach weiteren Loesungen
% sofort beendet wird. War notwendig, um zu verhindern, dass bei den Abfragen in
% Zeilen 20-22 Res mehrfach ausgegeben wird
or(true,_,true) :- !.
or(_,true,true) :- !.
or(false,false,false).

not(true,false).
not(false,true).

ex1(X,Y,Z,Res) :- and(X,Y,R1), or(R1,Z,Res).
ex2(X,Y,Z,Res) :- and(X,Y,R1), and(Y,Z,R2), or(R2,Z,R3), or(R1,R3,Res).
ex3(X,Y,Z,Res) :- not(Y,Y1), and(X,Y1,R1), and(R1,Z,R2), and(Z,Y,R3), or(R3,Z,R4), or(R2,R4,Res).

%Welche Ergebnisse erhalten Sie jeweils für die Werte X=true, Y=false und Z=true?
?- ex1(true,false,true,Res).
?- ex2(true,false,true,Res).
?- ex3(true,false,true,Res).

%Mit welcher Belegung erhalten Sie jeweils true als Ergebnis?
?- ex1(X,Y,Z,true).
?- ex2(X,Y,Z,true).
?- ex3(X,Y,Z,true).
