1.

DROP TABLE angajati_gma;

a)
CREATE TABLE angajati_gma (
    cod_ang NUMBER(4),
    nume VARCHAR2(20),
    prenume VARCHAR2(20),
    email CHAR(15),
    data_ang DATE,
    job VARCHAR2(10),
    cod_sef NUMBER(4),
    salariu NUMBER(8, 2),
    cod_dep NUMBER(2)
);

b)
CREATE TABLE angajati_gma (
    cod_ang NUMBER(4) PRIMARY KEY,
    nume VARCHAR2(20) NOT NULL,
    prenume VARCHAR2(20),
    email CHAR(15),
    data_ang DATE,
    job VARCHAR2(10),
    cod_sef NUMBER(4),
    salariu NUMBER(8, 2) NOT NULL,
    cod_dep NUMBER(2)
);

c)
CREATE TABLE angajati_gma (
    cod_ang NUMBER(4),
    nume VARCHAR2(20) NOT NULL,
    prenume VARCHAR2(20),
    email CHAR(15),
    data_ang DATE DEFAULT sysdate,
    job VARCHAR2(10),
    cod_sef NUMBER(4),
    salariu NUMBER(8, 2) NOT NULL,
    cod_dep NUMBER(2),
    PRIMARY KEY (cod_ang)
);


2.

INSERT INTO angajati_gma
VALUES (100, 'Nume1', 'Prenume1', null, null, 'Director', null, 20000, 10);
INSERT INTO angajati_gma
VALUES (101, 'Nume2', 'Prenume2', 'Nume2', '02-FEB-2004', 'Inginer', 100, 10000, 10);
INSERT INTO angajati_gma
VALUES (102, 'Nume3', 'Prenume3', 'Nume3', '05-JUN-2000', 'Analist', 101, 5000, 20);
INSERT INTO angajati_gma
VALUES (103, 'Nume4', 'Prenume4', null, null, 'Inginer', 100, 9000, 20);
INSERT INTO angajati_gma
VALUES (104, 'Nume5', 'Prenume5', 'Nume5', null, 'Analist', 101, 3000, 30);


3.

CREATE TABLE angajati10_gma AS
SELECT * FROM angajati_gma
WHERE cod_dep = 10;

DESCRIBE angajati10_gma;


4.

ALTER TABLE angajati_gma
ADD (comision NUMBER(4, 2));


5.

ALTER TABLE angajati_gma
MODIFY (comision NUMBER(6, 2));


6.

ALTER TABLE angajati_gma
MODIFY (salariu DEFAULT 3000);


7.

ALTER TABLE angajati_gma
MODIFY (
    comision NUMBER(2, 2),
    salariu NUMBER(10, 2)
);


8.

UPDATE angajati_gma
SET comision = 0.1
WHERE job LIKE 'A%';


9.

ALTER TABLE angajati_gma
MODIFY (email VARCHAR2(50));


10.

ALTER TABLE angajati_gma
ADD (nr_telefon NUMBER(10,0) DEFAULT 0700123456);


11.

SELECT * FROM angajati_gma;

ALTER TABLE angajati_gma
DROP COLUMN nr_telefon;

ROLLBACK;

12.

RENAME angajati_gma TO angajati3_gma;


13.

SELECT * FROM tab;

RENAME angajati3_gma TO angajati_gma;


14.

TRUNCATE TABLE angajati10_gma;


15.

CREATE TABLE departamente_gma (
    cod_dep NUMBER(2),
    nume VARCHAR2(15) NOT NULL,
    cod_director NUMBER(4)
);

DESC departamente_gma;


16.

INSERT INTO departamente_gma
COLUMNS (cod_dep, nume, cod_director)
VALUES (10, 'administrativ', 100);

INSERT INTO departamente_gma
COLUMNS (cod_dep, nume, cod_director)
VALUES (20, 'proiectare', 101);

INSERT INTO departamente_gma
COLUMNS (cod_dep, nume, cod_director)
VALUES (30, 'programare', null);

SELECT * FROM departamente_gma;


17.

ALTER TABLE departamente_gma
ADD PRIMARY KEY (cod_dep);


18.

ALTER TABLE angajati_gma
ADD FOREIGN KEY (cod_dep) REFERENCES departamente_gma(cod_dep);

DESC angajati_gma;


19.
20.

21.

Apare o eroare:
DROP TABLE departamente_gma;


22.

SELECT table_name FROM user_tables;

SELECT * FROM user_constraints;


23.

a)
SELECT constraint_name, constraint_type, table_name
FROM user_constraints
WHERE LOWER(table_name) IN ('angajati_gma', 'departamente_gma');

b)
SELECT table_name, constraint_name, column_name
FROM user_cons_columns
WHERE LOWER(table_name) IN ('angajati_gma', 'departamente_gma');


24.


SELECT * FROM angajati_gma;

UPDATE angajati_gma
SET email = 'example@example.com'
WHERE email IS null;

ALTER TABLE angajati_gma
MODIFY (email VARCHAR2(50) NOT NULL);


25.

INSERT INTO angajati_gma
COLUMNS (cod_ang, nume, prenume, email, job, cod_dep)
VALUES (110, 'Nume6', 'Prenume6', 'Nume6', 'Tester', 50);

Nu se poate deoarece nu există departamentul respectiv.


26.

INSERT INTO departamente_gma
COLUMNS (cod_dep, nume, cod_director)
VALUES (60, 'Analiza', null);

COMMIT;


27.

DELETE FROM departamente_gma
WHERE cod_dep = 20;

Nu se poate șterge acest departament, pentru că există angajați care îi aparțin.


28.

DELETE FROM departamente_gma
WHERE cod_dep = 60;

ROLLBACK;


29.

INSERT INTO angajati_gma
COLUMNS (cod_ang, nume, prenume, email, job, cod_dep, cod_sef)
VALUES (110, 'Nume6', 'Prenume6', 'Nume6', 'Tester', 60, 114);

Nu există șeful respectiv.


30.


INSERT INTO angajati_gma
COLUMNS (cod_ang, nume, prenume, email, job, cod_dep)
VALUES (114, 'Sef1', 'PrenumeSef1', 'exemplu', 'Sef', 60);

COMMIT;


31.

SELECT * FROM user_constraints
WHERE LOWER(table_name) = 'angajati_gma';

ALTER TABLE angajati_gma
DROP CONSTRAINT SYS_C00345312;

ALTER TABLE angajati_gma
ADD FOREIGN KEY (cod_dep) REFERENCES departamente_gma(cod_dep)
    ON DELETE CASCADE;


32.

COMMIT;

DELETE departamente_gma
WHERE cod_dep = 20;

SELECT * FROM angajati_gma;

ROLLBACK;


33.

ALTER TABLE departamente_gma
ADD FOREIGN KEY (cod_director) REFERENCES angajati_gma(cod_ang)
    ON DELETE SET NULL;


34.

COMMIT;

UPDATE departamente_gma
SET cod_director = 102
WHERE cod_dep = 30;

DELETE angajati_gma
WHERE cod_ang = 102;

SELECT * FROM angajati_gma;

SELECT * FROM departamente_gma;

ROLLBACK;


35.

ALTER TABLE angajati_gma
ADD CONSTRAINT salariu_mai_mic_35000 CHECK (salariu < 35000);


36.

UPDATE angajati_gma
SET salariu = 35000
WHERE cod_ang = 100;


37.

COMMIT;

ALTER TABLE angajati_gma
DROP CONSTRAINT salariu_mai_mic_35000;

ROLLBACK;

Nu putem reactiva constrângere, pentru că nu s-ar respecta condiția din CHECK.
