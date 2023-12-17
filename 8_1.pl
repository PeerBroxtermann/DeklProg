% Definition der Verheiratet-Relation:
verheiratet(christine, norbert).
verheiratet(hubert,    fritz).
verheiratet(monika,    angelika).
verheiratet(herbert,   maria).
verheiratet(claudia,   kim).

geschlecht(christine, w).
geschlecht(maria, w).
geschlecht(monika, w).
geschlecht(claudia, w).
geschlecht(anna, w).
geschlecht(susanne, w).
geschlecht(karolin, w).
geschlecht(angelika, d).
geschlecht(fritz, m).
geschlecht(hubert, m).
geschlecht(herbert, m).
geschlecht(kim, m).
geschlecht(andreas, m).
geschlecht(norbert, m).

% Definition der Kind-Eltern-Relation:
kind(herbert,  christine, norbert).
kind(angelika, christine, norbert).
kind(maria,    hubert,    fritz).
kind(karolin,  hubert,    fritz).
kind(susanne,  monika,    angelika).
kind(kim,      monika,    angelika).
kind(andreas,  herbert,   maria).
kind(anna,     claudia,   kim).

vater(Kind,Vater) :- kind(Kind,Vater,_), geschlecht(Vater,m).
vater(Kind,Vater) :- kind(Kind,_,Vater), geschlecht(Vater,m).

mutter(Kind,Mutter) :- kind(Kind,Mutter,_), geschlecht(Mutter,w).
mutter(Kind,Mutter) :- kind(Kind,_,Mutter), geschlecht(Mutter,w).

grossmutter(Person,Grossmutter) :- mutter(Person,Mutter), mutter(Mutter,Grossmutter), geschlecht(Grossmutter,w).

geschwister(Person,Geschwister) :- kind(Person,Person1,Person2), kind(Geschwister,Person1,Person2), Geschwister \= Person.

bruder(Person,Bruder) :- kind(Person,Person1,Person2), kind(Bruder,Person1,Person2), geschlecht(Bruder,m), Person \= Bruder.

tante(Person,Tante) :- vater(Person,Vater), geschwister(Vater,Tante), geschlecht(Tante,w).
tante(Person,Tante) :- mutter(Person,Mutter), geschwister(Mutter,Tante), geschlecht(Tante,w).
