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
