DROP TABLE expusa;
DROP TABLE fotografie;
DROP TABLE expozitie;
DROP TABLE artist;

CREATE TABLE artist (
    id_artist NUMBER(10) PRIMARY KEY,
    nume VARCHAR2(20) NOT NULL,
    data_nasterii DATE,
    nationalitate VARCHAR2(20)
);

CREATE TABLE expozitie (
    id_expozitie NUMBER(8) PRIMARY KEY,
    denumire VARCHAR2(40) NOT NULL,
    data_inceput DATE NOT NULL,
    data_sfarsit DATE NOT NULL,
    oras VARCHAR2(20) NOT NULL
);

CREATE TABLE fotografie (
    id_fotografie NUMBER(11) PRIMARY KEY,
    titlu VARCHAR2(50) NOT NULL,
    id_artist NUMBER(10) REFERENCES artist(id_artist),
    data_crearii DATE NOT NULL
);

CREATE TABLE expusa (
    id_fotografie NUMBER(11) REFERENCES fotografie(id_fotografie),
    id_expozitie NUMBER(8) REFERENCES expozitie(id_expozitie),
    data_crearii DATE NOT NULL,
    nr_zile NUMBER(10),
    CONSTRAINT pk_expusa PRIMARY KEY(id_fotografie, id_expozitie)
);

--ARTISTI
INSERT INTO artist
VALUES (1, 'Popescu Maria', TO_DATE('29.01.1997','DD.MM.YYYY'), 'Romana');

INSERT INTO artist
VALUES (2, 'Dragomir Darius', TO_DATE('01.05.1989','DD.MM.YYYY'), 'Italiana');

INSERT INTO artist
VALUES (3, 'Balaban Cristina', TO_DATE('06.11.1995','DD.MM.YYYY'), 'Franceza');

INSERT INTO artist
VALUES (4, 'Cornelius Cornel', TO_DATE('04.07.1994','DD.MM.YYYY'), NULL);

INSERT INTO artist
VALUES (5, 'Popescu Maria', TO_DATE('29.04.1989','DD.MM.YYYY'), 'Franceza');

INSERT INTO artist
VALUES (6, 'Spataru Sofia', TO_DATE('12.12.1997','DD.MM.YYYY'), 'Romana');

INSERT INTO artist
VALUES (7, 'Scoica Sorin', TO_DATE('29.01.1997','DD.MM.YYYY'), 'Italiana');

INSERT INTO artist
VALUES (8, 'Teodorescu Mihai', TO_DATE('16.06.1989','DD.MM.YYYY'), 'Romana');

INSERT INTO artist
VALUES (9, 'Vasilache Teodora', TO_DATE('18.01.1996','DD.MM.YYYY'), 'Germana');

INSERT INTO artist
VALUES (10, 'Oprescu Diana', NULL, NULL);

--EXPOZITII
INSERT INTO expozitie
VALUES (1, 'Fluturi zburatori', TO_DATE('24.12.2019','DD.MM.YYYY'), TO_DATE('29.12.2019','DD.MM.YYYY'), 'Bucuresti');

INSERT INTO expozitie
VALUES (2, 'Aiurart', TO_DATE('31.12.2019','DD.MM.YYYY'), TO_DATE('20.01.2020','DD.MM.YYYY'), 'Bucuresti');

INSERT INTO expozitie
VALUES (3, 'Go Contemporary', TO_DATE('11.01.2020','DD.MM.YYYY'), TO_DATE('15.01.2020','DD.MM.YYYY'), 'Londra');

INSERT INTO expozitie
VALUES (4, 'H’art Gallery', TO_DATE('04.10.2019','DD.MM.YYYY'), TO_DATE('09.10.2019','DD.MM.YYYY'), 'Paris');

INSERT INTO expozitie
VALUES (5, 'Mobius Gallery', TO_DATE('01.01.2020','DD.MM.YYYY'), TO_DATE('01.02.2020','DD.MM.YYYY'), 'Atena');

INSERT INTO expozitie
VALUES (6, 'Semiluna', TO_DATE('05.10.2019','DD.MM.YYYY'), TO_DATE('15.10.2019','DD.MM.YYYY'), 'Timisoara');

INSERT INTO expozitie
VALUES (7, 'Rapsodie de primavara', TO_DATE('15.10.2019','DD.MM.YYYY'), TO_DATE('17.10.2019','DD.MM.YYYY'), 'Praga');

INSERT INTO expozitie
VALUES (8, 'Rapsodie de primavara', TO_DATE('05.08.2019','DD.MM.YYYY'), TO_DATE('15.08.2019','DD.MM.YYYY'), 'Brasov');


--FOTOGRAFII
INSERT INTO fotografie
VALUES (1, 'Zborul spre nicaieri', 1, TO_DATE('09.09.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (2, 'My heart and I', 1, TO_DATE('31.12.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (3, 'Bonjour, mon amour', 2, TO_DATE('01.10.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (4, 'Derniere danse', 2, TO_DATE('03.10.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (5, 'Jucatorii de carti', 3, TO_DATE('22.12.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (6, 'Visul', 3, TO_DATE('19.09.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (7, 'Numarul 5', 3, TO_DATE('01.01.2020','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (8, 'Femeia III', 4, TO_DATE('09.01.2020','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (9, 'Adele Bloch-bauer I', 5, TO_DATE('10.01.2020','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (10, 'Tipatul', 6, TO_DATE('20.12.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (11, 'Bal du Moulin de la Galette', 7, TO_DATE('10.12.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (12, 'Baiatul cu pipa', 8, TO_DATE('06.10.2019','DD.MM.YYYY'));

INSERT INTO fotografie
VALUES (13, 'Irisi', 9, TO_DATE('07.08.2019','DD.MM.YYYY'));

--EXPUNERI ALE FOTOGRAFIILOR
--Bucuresti-gresit => TOO_MANY_ROWS
INSERT INTO expusa
VALUES (1, 1, TO_DATE('25.12.2019','DD.MM.YYYY'), 2);

INSERT INTO expusa
VALUES (2, 2, TO_DATE('01.01.2020','DD.MM.YYYY'), 10);

--Paris => are toate picturile in Paris => 2 bun pt Paris
INSERT INTO expusa
VALUES (3, 4, TO_DATE('06.10.2019','DD.MM.YYYY'), 3);

INSERT INTO expusa
VALUES (4, 4, TO_DATE('04.10.2019','DD.MM.YYYY'), 5);

--Atena => are toate picturile in Atena => 3 bun pt Atena
INSERT INTO expusa
VALUES (5, 5, TO_DATE('05.01.2020','DD.MM.YYYY'), 7);

INSERT INTO expusa
VALUES (6, 5, TO_DATE('30.01.2020','DD.MM.YYYY'), 1);

INSERT INTO expusa
VALUES (7, 5, TO_DATE('15.01.2020','DD.MM.YYYY'), 6);

--Londra => 4 bun pt Londra
INSERT INTO expusa
VALUES (8, 3, TO_DATE('11.01.2020','DD.MM.YYYY'), 3);

--6 nu e bun ca are in mai multe expozitii fotografiile
INSERT INTO expusa
VALUES (10, 2, TO_DATE('01.01.2020','DD.MM.YYYY'), 5);

INSERT INTO expusa
VALUES (10, 1, TO_DATE('25.12.2019','DD.MM.YYYY'), 3);

INSERT INTO expusa
VALUES (10, 5, TO_DATE('17.01.2020','DD.MM.YYYY'), 7);

--7 bun pt Atena
INSERT INTO expusa
VALUES (11, 5, TO_DATE('10.01.2020','DD.MM.YYYY'), 4);

--8 bun pt Paris
INSERT INTO expusa
VALUES (12, 4, TO_DATE('05.10.2019','DD.MM.YYYY'), 2);

--9 bun pt Paris
INSERT INTO expusa
VALUES (13, 4, TO_DATE('05.10.2019','DD.MM.YYYY'), 1);

COMMIT;
