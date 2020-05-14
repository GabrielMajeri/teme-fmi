27.

WITH profit AS (
    SELECT
        cod_agentie, destinatie,
        pret * (1 - NVL(discount, 0)) AS profit
    FROM excursie
    INNER JOIN achizitioneaza
    ON id_excursie = cod_excursie
)
SELECT cod_agentie, destinatie, SUM(profit)
FROM profit
GROUP BY GROUPING SETS (
    (cod_agentie, destinatie),
    (cod_agentie),
    (destinatie),
    ()
);


28.

WITH profit AS (
    SELECT
        cod_agentie, TO_CHAR(data_achizitie, 'YYYY') AS an,
        pret * (1 - NVL(discount, 0)) AS profit
    FROM excursie
    INNER JOIN achizitioneaza
    ON id_excursie = cod_excursie
    WHERE cod_agentie IS NOT NULL
)
SELECT cod_agentie, an, SUM(profit)
FROM profit
GROUP BY GROUPING SETS (
    (cod_agentie, an),
    ()
);


29.

SELECT ex.denumire
FROM excursie ex
WHERE ex.cod_agentie IS NULL
AND NOT EXISTS (
    SELECT 1
    FROM turist
    INNER JOIN achizitioneaza
    ON cod_turist = id_turist
    WHERE (TO_CHAR(data_nastere, 'YYYY') = '1984')
        AND (cod_excursie = ex.id_excursie)
);


30.

CREATE TABLE turist_gma AS SELECT * FROM turist;
ALTER TABLE turist_gma
ADD PRIMARY KEY (id_turist);

CREATE TABLE achizitioneaza_gma AS SELECT * FROM achizitioneaza;
ALTER TABLE achizitioneaza_gma
ADD FOREIGN KEY (cod_turist) REFERENCES turist_gma (id_turist)
ON DELETE CASCADE;
ALTER TABLE achizitioneaza_gma
ADD FOREIGN KEY (cod_excursie) REFERENCES excursie_gma (id_excursie)
ON DELETE CASCADE;

CREATE TABLE excursie_gma AS SELECT * FROM excursie;
ALTER TABLE excursie_gma
ADD PRIMARY KEY (id_excursie);
ALTER TABLE excursie_gma
ADD FOREIGN KEY (cod_agentie) REFERENCES agentie_gma (id_agentie)
ON DELETE CASCADE;

CREATE TABLE agentie_gma AS SELECT * FROM agentie;
ALTER TABLE agentie_gma
ADD PRIMARY KEY (id_agentie);


31.

COMMIT;

DESC excursie_gma;

UPDATE achizitioneaza_gma
SET discount = (SELECT MAX(discount) FROM achizitioneaza_gma)
WHERE (
    SELECT pret
    FROM excursie_gma
    WHERE id_excursie = cod_excursie
) > (SELECT AVG(pret) FROM excursie_gma);

ROLLBACK;


32.

DELETE FROM excursie_gma e
WHERE e.pret < (
    SELECT AVG(pret)
    FROM excursie_gma
    WHERE cod_agentie = e.cod_agentie
);

ROLLBACK;


33.

UPDATE excursie_gma
SET cod_agentie = null
WHERE cod_agentie NOT IN (
    SELECT DISTINCT cod_agentie
    FROM agentie_gma
);


34.

CREATE VIEW v_excursie_gma AS
    SELECT *
    FROM excursie_gma
    WHERE cod_agentie = 10
    WITH CHECK OPTION;

SELECT * FROM v_excursie_gma;

INSERT INTO v_excursie_gma
VALUES (110, 'Test', 2000, 'Spania', 5, 20, 15);

COMMIT;


35.

TRUNCATE TABLE achizitioneaza_gma;
SAVEPOINT a;


36.

INSERT INTO achizitioneaza_gma
    SELECT *
    FROM achizitioneaza
    WHERE TO_CHAR(data_achizitie, 'YYYY') = '2010';

UPDATE achizitioneaza_gma
SET data_start = data_start + 31,
    data_end = data_end + 31;


37.

UPDATE achizitioneaza_gma
SET discount = 1.1 * discount
WHERE (
    SELECT cod_agentie
    FROM excursie_gma
    WHERE cod_excursie = id_excursie
) = 10;


38.

DELETE FROM achizitioneaza_gma
WHERE (
    SELECT data_nastere
    FROM turist_gma
    WHERE id_turist = cod_turist
) IS NULL;


39.

INSERT INTO achizitioneaza_gma
SELECT * FROM achizitioneaza;


39.

UPDATE excursie_gma
SET pret = 0.9 * pret
WHERE id_excursie IN (
    SELECT cod_excursie
    FROM (
        SELECT cod_excursie, COUNT(1) AS nr_achizitii
        FROM achizitioneaza_gma
        GROUP BY cod_excursie
    ) achizitii
    WHERE nr_achizitii = 2
);


40.

ALTER TABLE turist_gma
MODIFY (nume NOT NULL);

ALTER TABLE turist_gma
ADD UNIQUE (nume, prenume);


41.

ALTER TABLE achizitioneaza_gma
ADD CHECK (data_start < data_end);

ALTER TABLE achizitioneaza_gma
MODIFY (data_achizitie DEFAULT sysdate);


42.

SELECT *
FROM achizitioneaza_gma
WHERE data_start > sysdate
FOR UPDATE OF data_achizitie;

UPDATE achizitioneaza_gma
SET data_achizitie = DEFAULT
WHERE data_start > sysdate;

COMMIT;


43.

WITH excursii_stanciu AS (
    SELECT DISTINCT cod_excursie
    FROM excursie_gma
    INNER JOIN achizitioneaza_gma
    ON cod_excursie = id_excursie
    INNER JOIN turist_gma
    ON cod_turist = id_turist
    WHERE nume = 'Stanciu'
)
SELECT t.nume, t.prenume
FROM turist t
WHERE NOT EXISTS (
    (
        SELECT cod_excursie
        FROM excursii_stanciu
    )
    MINUS
    (
        SELECT cod_excursie
        FROM achizitioneaza_gma
        WHERE cod_turist = t.id_turist
    )
);


44.

WITH excursii_stanciu AS (
    SELECT DISTINCT cod_excursie
    FROM excursie_gma
    INNER JOIN achizitioneaza_gma
    ON cod_excursie = id_excursie
    INNER JOIN turist_gma
    ON cod_turist = id_turist
    WHERE nume = 'Stanciu'
)
SELECT t.nume, t.prenume
FROM turist t
WHERE NOT EXISTS (
    SELECT cod_excursie
    FROM achizitioneaza_gma
    WHERE cod_turist = t.id_turist AND
        cod_excursie NOT IN (
            SELECT cod_excursie
            FROM excursii_stanciu
        )
);


45.

WITH excursii_stanciu AS (
    SELECT DISTINCT cod_excursie
    FROM excursie_gma
    INNER JOIN achizitioneaza_gma
    ON cod_excursie = id_excursie
    INNER JOIN turist_gma
    ON cod_turist = id_turist
    WHERE nume = 'Stanciu'
)
SELECT t.nume, t.prenume
FROM turist t
WHERE NOT EXISTS (
    (
        SELECT cod_excursie
        FROM achizitioneaza_gma
        WHERE cod_turist = t.id_turist
    )
    MINUS
    (
        SELECT cod_excursie
        FROM excursii_stanciu
    )
) AND NOT EXISTS (
    (
        SELECT cod_excursie
        FROM excursii_stanciu
    )
    MINUS
    (
        SELECT cod_excursie
        FROM achizitioneaza_gma
        WHERE cod_turist = t.id_turist
    )
);


46.

INSERT INTO turist_gma
VALUES (&id_turist, &nume_turist, null, sysdate);


47.

UPDATE turist_gma
SET prenume = 'Ion'
WHERE id_turist = 100;

SELECT nume, prenume
FROM turist_gma
WHERE id_turist = 100;


48.

DELETE FROM turist_gma
WHERE id_turist = 100;
