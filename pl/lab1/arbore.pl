%---------------------------------
% Jon Snow and Daenerys Targaryen
%---------------------------------

male(rickardStark).
male(eddardStark).
male(brandonStark).
male(benjenStark).
male(robbStark).
male(branStark).
male(rickonStark).
male(aerysTargaryen).
male(rhaegarTargaryen).
male(viserysTargaryen).
male(aegonTargaryen).
male(jonSnow).

%---------------------------

female(lyarraStark).
female(catelynStark).
female(lyannaStark).
female(sansaStark).
female(aryaStark).
female(rhaellaTargaryen).
female(eliaTargaryen).
female(daenerysTargaryen).
female(rhaenysTargaryen).

%------------------------

/* Copii lui Rickard & Lyarra */
parent_of(rickardStark, eddardStark).
parent_of(lyarraStark, eddardStark).
parent_of(rickardStark, brandonStark).
parent_of(lyarraStark, brandonStark).
parent_of(rickardStark, benjenStark).
parent_of(lyarraStark, benjenStark).
parent_of(rickardStark, lyannaStark).
parent_of(lyarraStark, lyannaStark).

/* Copii lui Aerys & Rhaella */
parent_of(aerysTargaryen, rhaegarTargaryen).
parent_of(rhaellaTargaryen, rhaegarTargaryen).
parent_of(aerysTargaryen, eliaTargaryen).
parent_of(rhaellaTargaryen, eliaTargaryen).
parent_of(aerysTargaryen, viserysTargaryen).
parent_of(rhaellaTargaryen, viserysTargaryen).
parent_of(aerysTargaryen, daenerysTargaryen).
parent_of(rhaellaTargaryen, daenerysTargaryen).

/* Copii lui Ned & Catelyn */
parent_of(eddardStark, robbStark).
parent_of(catelynStark, robbStark).
parent_of(eddardStark, sansaStark).
parent_of(catelynStark, sansaStark).
parent_of(eddardStark, aryaStark).
parent_of(catelynStark, aryaStark).
parent_of(eddardStark, branStark).
parent_of(catelynStark, branStark).
parent_of(eddardStark, rickonStark).
parent_of(catelynStark, rickonStark).

/* Copii lui Rhaegar & Elia */
parent_of(rhaegarTargaryen, rhaenysTargaryen).
parent_of(eliaTargaryen, rhaenysTargaryen).
parent_of(rhaegarTargaryen, aegonTargaryen).
parent_of(eliaTargaryen, aegonTargaryen).

/* Părinții lui Jon Snow */
parent_of(lyannaStark, jonSnow).
parent_of(rhaegarTargaryen, jonSnow).

/* Tatăl este părintele de gen masculin. */
father_of(Father, Child) :- male(Father), parent_of(Father, Child).
/* Mama este părintele de gen feminin. */
mother_of(Mother, Child) :- female(Mother), parent_of(Mother, Child).

/* Bunicul este tatăl unuia dintre părinți. */
grandfather_of(Grandfather, Child) :- father_of(Grandfather, Parent), parent_of(Parent, Child).
/* Bunica este mama unuia dintre părinți. */
grandmother_of(Grandmother, Child) :- mother_of(Grandmother, Parent), parent_of(Parent, Child).

/* Iau în considerare doar frații/surorile nevitrege. Trebuie să aibă exact aceiași părinți. */
sibling_of(Sibling, Person) :-
    mother_of(Mother, Sibling), mother_of(Mother, Person),
    father_of(Father, Sibling), father_of(Father, Person),
    /* O persoană are aceiași părinți cu ea însăși, dar nu vreau să apară în relație. */
    Sibling \= Person.

sister_of(Sister, Person) :- sibling_of(Sister, Person), female(Sister).
brother_of(Brother, Person) :- sibling_of(Brother, Person), male(Brother).

/* Mătușă înseamnă să fie sora unuia dintre părinți. */
aunt_of(Aunt, Person) :- sister_of(Aunt, Parent), parent_of(Parent, Person).
/* Unchi înseamnă să fie fratele unuia dintre părinți. */
uncle_of(Uncle, Person) :- brother_of(Uncle, Parent), parent_of(Parent, Person).


/* Pentru spoilere, încercați interogarea `aunt_of(Aunt, jonSnow)`. */



/* Definiția recursivă a strămoșului. */
ancestor_of(X, Y) :- parent_of(X, Y).
ancestor_of(X, Y) :- parent_of(X, Z), ancestor_of(Z, Y).



/* `not_parent` greșit, care nu dă răspunsuri la toate întrebările. */
% not_parent(X, Y) :- not(parent_of(X, Y)).

/* Pentru a putea enumera toate persoanele din baza de date,
 * definesc un predicat nou.
 */
person(X) :- male(X); female(X).
/* Definesc `not_parent` într-un mod exaustiv. Prolog va căuta
 * prin toate perechiile posibile de persoane și va verifica ca prima
 * să nu fie părintele celei de a doua.
 */
not_parent(X, Y) :- person(X), person(Y), \+ parent_of(X, Y).



/* Să nu fie părinte dar să fie strămoș. */
ancestor_not_parent(X, Y) :- not_parent(X, Y), ancestor_of(X, Y).
