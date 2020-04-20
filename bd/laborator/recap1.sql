1.

SELECT *
FROM (
    SELECT e.denumire
    FROM excursie e
    INNER JOIN achizitioneaza a
    ON e.id_excursie = a.cod_excursie
    ORDER BY data_achizitie
)
WHERE ROWNUM = 1;


2.

SELECT cod_excursie, COUNT(1)
FROM achizitioneaza
GROUP BY cod_excursie;


3.

WITH nr_excursii AS (
    SELECT
        ag.denumire, ag.oras,
        COUNT(1) AS nr_excursii
    FROM agentie ag
    INNER JOIN excursie ex
    ON ex.cod_agentie = ag.id_agentie
    GROUP BY ag.denumire, ag.oras),
pret_mediu AS (
    SELECT
        ag.denumire, ag.oras,
        AVG(ex.pret) AS pret_mediu
    FROM agentie ag
    INNER JOIN excursie ex
    ON ex.cod_agentie = ag.id_agentie
    INNER JOIN achizitioneaza ac
    ON ac.cod_excursie = ex.id_excursie
    GROUP BY ag.denumire, ag.oras)
SELECT denumire, oras, nr_excursii, pret_mediu
FROM nr_excursii
INNER JOIN pret_mediu
USING (denumire, oras);


4.

WITH nr_excursii AS (
    SELECT t.nume, t.prenume, COUNT(1) AS nr
    FROM turist t
    INNER JOIN achizitioneaza a
    ON t.id_turist = a.cod_turist
    GROUP BY t.nume, t.prenume)
SELECT nume, prenume
FROM nr_excursii
WHERE nr >= 2;

WITH nr_excursii AS (
    SELECT t.nume, t.prenume, COUNT(1) AS nr
    FROM turist t
    INNER JOIN achizitioneaza a
    ON t.id_turist = a.cod_turist
    GROUP BY t.nume, t.prenume)
SELECT COUNT(1) AS nr_turisti
FROM nr_excursii;


5.

WITH turisti_paris AS (
    SELECT *
    FROM turist t
    INNER JOIN achizitioneaza a
    ON t.id_turist = a.cod_turist
    INNER JOIN excursie e
    ON a.cod_excursie = e.id_excursie
    WHERE e.destinatie LIKE 'Paris')
SELECT *
FROM turist
WHERE (nume, prenume) NOT IN (
    SELECT nume, prenume
    FROM turisti_paris);

6.

SELECT id_turist, nume, prenume
FROM turist t1
WHERE 2 <= (
    SELECT COUNT(DISTINCT destinatie)
    FROM turist t2
    INNER JOIN achizitioneaza
    ON t2.id_turist = cod_turist
    INNER JOIN excursie
    ON id_excursie = cod_excursie
    WHERE t1.id_turist = t2.id_turist
);


7. 

SELECT
    denumire,
    NVL(
        (SELECT SUM(pret - pret * NVL(discount, 0)) AS profit
        FROM achizitioneaza
        INNER JOIN excursie
        ON cod_excursie = id_excursie
        WHERE cod_agentie = id_agentie),
    0) AS profit
FROM agentie;


8.

SELECT denumire, oras
FROM agentie
WHERE 3 <= (
    SELECT COUNT(1)
    FROM excursie
    WHERE cod_agentie = id_agentie AND pret < 2000
);


9.

SELECT id_excursie, denumire
FROM excursie
WHERE id_excursie NOT IN (
    SELECT DISTINCT cod_excursie
    FROM achizitioneaza
);


10. 

SELECT
    e.denumire AS denumire_excursie,
    e.pret,
    NVL(a.denumire, 'agentie necunoscuta') AS denumire_agentie
FROM excursie e
LEFT OUTER JOIN agentie a
ON e.cod_agentie = a.id_agentie;


11. 

SELECT denumire, pret
FROM excursie
WHERE pret > (
    SELECT pret
    FROM excursie
    WHERE denumire LIKE 'Orasul luminilor' AND cod_agentie = 10
);


12.

SELECT nume, prenume, (data_end - data_start) AS durata
FROM turist
INNER JOIN achizitioneaza
ON id_turist = cod_turist
WHERE (data_end - data_start) >= 10;


13.

SELECT DISTINCT id_excursie
FROM excursie
INNER JOIN achizitioneaza
ON cod_excursie = id_excursie
INNER JOIN turist
ON cod_turist = id_turist
WHERE (SYSDATE - data_nastere)/365.25 <= 35;


14.

SELECT nume, prenume
FROM turist
WHERE id_turist NOT IN (
    SELECT DISTINCT id_turist
    FROM turist
    INNER JOIN achizitioneaza
    ON cod_turist = id_turist
    INNER JOIN excursie
    ON cod_excursie = id_excursie
    INNER JOIN agentie
    ON cod_agentie = id_agentie
    WHERE oras LIKE 'Bucuresti'
);


15.

SELECT nume, prenume
FROM turist
WHERE id_turist IN (
    SELECT DISTINCT id_turist
    FROM turist
    INNER JOIN achizitioneaza
    ON cod_turist = id_turist
    INNER JOIN excursie
    ON cod_excursie = id_excursie
    INNER JOIN agentie
    ON cod_agentie = id_agentie
    WHERE (excursie.denumire LIKE '%1 Mai%') AND (agentie.oras LIKE 'Bucuresti')
);


16.

SELECT nume, prenume, excursie.denumire
FROM turist
INNER JOIN achizitioneaza
ON cod_turist = id_turist
INNER JOIN excursie
ON cod_excursie = id_excursie
INNER JOIN agentie
ON cod_agentie = id_agentie
WHERE agentie.denumire LIKE 'Smart Tour';


17.

SELECT *
FROM (
    SELECT id_excursie, nr_locuri, COUNT(1) AS locuri_ocupate
    FROM achizitioneaza
    INNER JOIN excursie
    ON cod_excursie = id_excursie
    WHERE data_start = '14-AUG-2011'
    GROUP BY id_excursie, nr_locuri
) nr_locuri
WHERE nr_locuri = locuri_ocupate;


18.

WITH date_achizitie AS (
    SELECT id_turist, id_excursie, data_achizitie
    FROM excursie
    INNER JOIN achizitioneaza
    ON cod_excursie = id_excursie
    INNER JOIN turist
    ON cod_turist = id_turist
    ORDER BY data_achizitie DESC
)
SELECT id_turist, MAX(data_achizitie)
FROM date_achizitie
GROUP BY id_turist
ORDER BY id_turist;


19.

SELECT *
FROM (
    SELECT denumire, pret
    FROM excursie
    ORDER BY pret DESC
) top_excursii
WHERE ROWNUM <= 5;


20.

SELECT nume, prenume
FROM turist
INNER JOIN achizitioneaza
ON id_turist = cod_turist
WHERE TO_CHAR(data_achizitie, 'MON') = TO_CHAR(data_nastere, 'MON');


21.

WITH nr_persoane AS (
    SELECT cod_excursie, COUNT(1) AS nr
    FROM achizitioneaza
    GROUP BY cod_excursie
),
doua_persoane AS (
    SELECT *
    FROM nr_persoane
    WHERE nr = 2
)
SELECT DISTINCT nume, prenume
FROM turist
INNER JOIN achizitioneaza
ON id_turist = cod_turist
INNER JOIN doua_persoane
USING (cod_excursie)
INNER JOIN excursie
ON id_excursie = cod_excursie
INNER JOIN agentie
ON id_agentie = cod_agentie
WHERE oras LIKE 'Constanta';


22.

WITH d AS (
    SELECT
        id_excursie,
        CASE
        WHEN durata <= 5 THEN
            'mica'
        WHEN durata <= 19 THEN
            'medie'
        ELSE
            'lunga'
        END AS durata_text,
        durata
    FROM excursie
)
SELECT id_excursie, durata_text
FROM d
ORDER BY durata;


23.

WITH nr_excursii AS (
    SELECT oras, COUNT(1) AS nr
    FROM agentie
    INNER JOIN excursie
    ON id_agentie = cod_agentie
    GROUP BY oras
)
SELECT
    (
        SELECT SUM(nr)
        FROM nr_excursii
    ) AS "Numar excursii",
    (
        SELECT nr
        FROM nr_excursii
        WHERE oras LIKE 'Constanta'
    ) AS "Nr. ex Contanta",
    (
        SELECT nr
        FROM nr_excursii
        WHERE oras LIKE 'Bucuresti'
    ) AS "Nr. ex Bucuresti"
FROM dual;


24.

SELECT id_excursie, denumire
FROM excursie
INNER JOIN achizitioneaza
ON id_excursie = cod_excursie
INNER JOIN turist
ON id_turist = cod_turist
WHERE ROUND((SYSDATE - data_nastere) / 365.25) = 35;


25.

SELECT
    id_agentie,
    destinatie,
    SUM(pret - pret * NVL(discount, 0)) AS profit,
    GROUPING(id_agentie),
    GROUPING(destinatie)
FROM agentie
INNER JOIN excursie
ON id_agentie = cod_agentie
INNER JOIN achizitioneaza
ON id_excursie = cod_excursie
GROUP BY GROUPING SETS (
    (id_agentie, destinatie),
    (id_agentie),
    ()
);


26. 

WITH preturi_medii AS (
    SELECT id_agentie, oras, AVG(pret) AS pret_mediu
    FROM agentie
    INNER JOIN excursie
    ON id_agentie = cod_agentie
    GROUP BY id_agentie, oras
)
SELECT
    agentie.oras,
    agentie.id_agentie,
    preturi_medii.id_agentie AS id_concurent,
    preturi_medii.pret_mediu AS pret_mediu_concurent
FROM agentie
LEFT OUTER JOIN preturi_medii
ON agentie.oras = preturi_medii.oras
    AND agentie.id_agentie != preturi_medii.id_agentie;
