fotografie(id_fotografie, titlu, id_artist, data_crearii)
expusa(id_fotografie, id_expozitie, data_crearii, nr_zile)
expozitie(id_expozitie, denumire, data_inceput, data_sfarsit, oras);
artist(id_artist, nume, data_nasterii, nationalitate);

1. Sa se afiseze fotografiile artistilor de nationalitate romana
care si-au expus fotografiile cel putin o treime din durata expozitiei.

2. Sa se afiseze in ordine alfabetica dupa nume artistii care
nu au nicio fotografie.

3. Sa se afiseze expozitiile care au avut expuse cel putin 2 fotografii de la artisti diferiti. 

4. Sa se stearga expunerile expozitiilor care au avut cel putin 2 fotografii.
Anulati modificarile.

5. Sa se adauge coloana id_artist in tabelul expozitie care va permite sa 
se cunoasca artistul care a organizat expozitia. Coloana va fi adaugata impreuna
cu o constrangere de cheie externa.
