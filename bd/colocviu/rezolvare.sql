1. Sa se afiseze fotografiile artistilor de nationalitate romana
care si-au expus fotografiile cel putin o treime din durata expozitiei.

SELECT *
FROM fotografie
INNER JOIN artist USING (id_artist)
WHERE id_artist IN (
    SELECT id_artist
    FROM artist
    INNER JOIN fotografie USING (id_artist)
    INNER JOIN expusa USING (id_fotografie)
    INNER JOIN expozitie USING (id_expozitie)
    WHERE nr_zile >= (data_sfarsit - data_inceput) / 3
) AND nationalitate = 'Romana';


2. Sa se afiseze in ordine alfabetica dupa nume artistii care
nu au nicio fotografie.

SELECT DISTINCT id_artist, nume
FROM artist
LEFT OUTER JOIN fotografie USING (id_artist)
WHERE id_fotografie IS NULL
ORDER BY nume;


3. Sa se afiseze expozitiile care au avut expuse
cel putin 2 fotografii de la artisti diferiti. 

WITH artisti_per_expozitie AS (
    SELECT id_expozitie, (
        SELECT COUNT (DISTINCT id_artist)
        FROM expusa
        INNER JOIN fotografie USING (id_fotografie)
        WHERE id_expozitie = e.id_expozitie
    ) AS num_artisti
    FROM expozitie e
)
SELECT *
FROM expozitie
INNER JOIN artisti_per_expozitie USING (id_expozitie)
WHERE num_artisti >= 2;


4. Sa se stearga expunerile expozitiilor care au avut cel putin 2 fotografii.
Anulati modificarile.

COMMIT;

DELETE FROM expusa
WHERE id_expozitie IN (
    SELECT id_expozitie
    FROM fotografie
    INNER JOIN expusa USING (id_fotografie)
    GROUP BY (id_expozitie)
    HAVING COUNT(1) >= 2
);

ROLLBACK;


5. Sa se adauge coloana id_artist in tabelul expozitie care va permite sa 
se cunoasca artistul care a organizat expozitia. Coloana va fi adaugata impreuna
cu o constrangere de cheie externa.

ALTER TABLE expozitie
ADD id_artist NUMBER
CONSTRAINT expozitie_artist REFERENCES artist(id_artist);
