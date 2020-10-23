SET SERVEROUTPUT ON;
SET VERIFY OFF;

---- Exerciții
-- 1
DECLARE
    numar   NUMBER(3) := 100;
    mesaj1  VARCHAR2(255) := 'text 1';
    mesaj2  VARCHAR2(255) := 'text 2';
BEGIN
    DECLARE
        numar   NUMBER(3) := 1;
        mesaj1  VARCHAR2(255) := 'text 2';
        mesaj2  VARCHAR2(255) := 'text 3';
    BEGIN
        numar := numar + 1;
        -- a) numar este 2 aici
        DBMS_OUTPUT.PUT_LINE(numar);
        -- b) mesaj1 este 'text 2' aici
        DBMS_OUTPUT.PUT_LINE(mesaj1);
        mesaj2 := mesaj2 || ' adaugat in sub-bloc';
        -- c) mesaj2 este 'text 3 adaugat in sub-bloc' aici
        DBMS_OUTPUT.PUT_LINE(mesaj2);
    END;

    numar := numar + 1;
    -- d) numar este 101 aici
    DBMS_OUTPUT.PUT_LINE(numar);
    mesaj1 := mesaj1 || ' adaugat in blocul principal';
    -- e) mesaj1 este 'text 1 adaugat in blocul principal'
    DBMS_OUTPUT.PUT_LINE(mesaj1);
    mesaj2 := mesaj2 || ' adaugat in blocul principal';
    -- f) mesaj2 este 'text 2 adaugat in blocul principal' aici
    DBMS_OUTPUT.PUT_LINE(mesaj2);
END;
/

-- 2
SELECT all_days.book_date, NVL(days_with_rentals.num, 0) AS num
FROM (
    -- Generate a list of all days with bookings
    SELECT TO_DATE(book_date) AS book_date, COUNT(1) AS num
    FROM rental
    GROUP BY book_date
) days_with_rentals
RIGHT JOIN (
    -- Generate all the days in october
    SELECT TO_DATE(TRUNC (last_day('01-OCT-2020') - ROWNUM)) AS book_date
    FROM DUAL CONNECT BY ROWNUM < 31
) all_days
ON days_with_rentals.book_date = all_days.book_date
ORDER BY book_date;


DROP TABLE zile_octombrie;
CREATE TABLE zile_octombrie (
    id NUMBER PRIMARY KEY,
    data DATE NOT NULL UNIQUE
);
COMMIT;

DECLARE
    ziua DATE := '01-OCT-2020';
BEGIN
    FOR i IN 1..31 LOOP
        INSERT INTO zile_octombrie VALUES (i, ziua);
        ziua := ziua + 1;
    END LOOP;
END;
/

SELECT data, NVL(num, 0)
FROM zile_octombrie
LEFT JOIN (
    SELECT TO_DATE(book_date) AS data, COUNT(1) AS num
    FROM rental
    GROUP BY book_date
)
USING (data)
ORDER BY data;

-- 3
UNDEFINE nume;
DECLARE
    searched_member NUMBER;
    num_movies NUMBER;
BEGIN
    SELECT member_id INTO searched_member
    FROM member
    WHERE first_name = &name;

    SELECT COUNT(*) INTO num_movies
    FROM title
    INNER JOIN rental
    USING (title_id)
    WHERE member_id = searched_member;

    IF num_movies = 0 THEN
        DBMS_OUTPUT.PUT_LINE('No movies rented');
    ELSE
        DBMS_OUTPUT.PUT_LINE('Rented: ' || num_movies);
    END IF;

EXCEPTION
    WHEN too_many_rows THEN
        DBMS_OUTPUT.PUT_LINE('Multiple people with same name');
END;
/


-- 4
UNDEFINE nume;
DECLARE
    searched_member NUMBER;
    num_movies NUMBER;
    total_movies NUMBER;
    percent_rented NUMBER;
BEGIN
    SELECT member_id INTO searched_member
    FROM member
    WHERE first_name = &name;

    SELECT COUNT(*) INTO num_movies
    FROM title
    INNER JOIN rental
    USING (title_id)
    WHERE member_id = searched_member;

    IF num_movies = 0 THEN
        DBMS_OUTPUT.PUT_LINE('No movies rented');
    ELSE
        DBMS_OUTPUT.PUT_LINE('Rented: ' || num_movies);
    END IF;

    SELECT COUNT(*) INTO total_movies
    FROM title;

    percent_rented := num_movies / total_movies;
    IF percent_rented >= 0.75 THEN
        DBMS_OUTPUT.PUT_LINE('Category 1');
    ELSIF percent_rented >= 0.5 THEN
        DBMS_OUTPUT.PUT_LINE('Category 2');
    ELSIF percent_rented >= 0.25 THEN
        DBMS_OUTPUT.PUT_LINE('Category 3');
    ELSE
        DBMS_OUTPUT.PUT_LINE('Category 4');
    END IF;
EXCEPTION
    WHEN too_many_rows THEN
        DBMS_OUTPUT.PUT_LINE('Multiple people with same name');
END;
/

-- 5
CREATE TABLE member_with_discount AS (
    SELECT * FROM member
);

ALTER TABLE member_with_discount
ADD discount NUMBER DEFAULT NULL;

UNDEFINE cod_membru;
DECLARE
    searched_member NUMBER;
    num_movies NUMBER;
    total_movies NUMBER;
    percent_rented NUMBER;
    original_discount NUMBER;
    new_discount NUMBER;
BEGIN
    searched_member := &cod_membru;

    SELECT COUNT(*) INTO num_movies
    FROM title
    INNER JOIN rental
    USING (title_id)
    WHERE member_id = searched_member;

    SELECT COUNT(*) INTO total_movies
    FROM title;


    SELECT discount INTO original_discount
    FROM member_with_discount
    WHERE member_id = searched_member;

    percent_rented := num_movies / total_movies;
    IF percent_rented >= 0.75 THEN
        new_discount := 0.10;
    ELSIF percent_rented >= 0.5 THEN
        new_discount := 0.05;
    ELSIF percent_rented >= 0.25 THEN
        new_discount := 0.03;
    ELSE
        new_discount := NULL;
    END IF;

    IF NVL(original_discount, 0) != NVL(new_discount, 0) THEN
        UPDATE member_with_discount
        SET discount = new_discount
        WHERE member_id = searched_member;

        DBMS_OUTPUT.PUT_LINE('Updated discount to be ' || new_discount);
    ELSE
        DBMS_OUTPUT.PUT_LINE('Discount is unchanged');
    END IF;
END;
/
