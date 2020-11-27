-- N = 57

-- Cerere în limbaj natural: fiind dat un șir de caractere, să se caute toate
-- locațiile care conțin acel șir în numele străzii.
-- Să se afișeze pentru fiecare locație, toate departamentele și toți angajații
-- care lucrează acolo.
--
-- Se va afișa: ID-ul locației, numele departamentului, ID-ul angajatului, numele lui


-- Procedura care rezolvă cererea și afișează rezultatul.
CREATE OR REPLACE PROCEDURE cauta_angajati_locatie (
    subsir IN VARCHAR
) IS
    v_nr_locatii NUMBER;
BEGIN
    -- Verific dacă există locația
    SELECT COUNT(1)
    INTO v_nr_locatii
    FROM locations
    WHERE LOWER(street_address) LIKE ('%' || LOWER(subsir) || '%');

    -- Nu am găsit-o, afișez un mesaj și returnez.
    IF v_nr_locatii = 0 THEN
        DBMS_OUTPUT.PUT_LINE('Nu am gasit locatia cautata');
        RETURN;
    END IF;

    -- Folosesc un ciclu cursor cu subcereri
    FOR c IN (
        SELECT location_id, department_name, employee_id, last_name
        FROM locations
        LEFT OUTER JOIN departments
        USING (location_id)
        LEFT OUTER JOIN employees
        USING (department_id)
        WHERE LOWER(street_address) LIKE ('%' || LOWER(subsir) || '%')
    )
    LOOP
        -- Dacă numele departamentului e NULL, știm că
        -- nu există departament la această locație
        IF c.department_name IS NULL THEN
            DBMS_OUTPUT.PUT_LINE(c.location_id || '-nu are departamente');

        -- Dacă ID-ul angajatului e NULL, știm că nu există angajați
        -- ai acestui departament.
        ELSIF c.employee_id IS NULL THEN
            DBMS_OUTPUT.PUT_LINE(c.location_id || '-' ||
                c.department_name || ' nu are angajati');

        -- Afișăm în mod obișnuit angajații.
        ELSE
            DBMS_OUTPUT.PUT_LINE(c.location_id || '-' || c.department_name ||
                ': ' || c.employee_id || ' ' || c.last_name);
        END IF;
    END LOOP;
END;
/

-- Exemplu de apelare
BEGIN
    -- Un exemplu în care găsește locații
    cauta_angajati_locatie('St');

    -- Las un rând liber
    DBMS_OUTPUT.PUT_LINE('');

    -- Un exemplu în care nu găsește locații, și tratează cazul de eroare
    cauta_angajati_locatie('XYZ');
END;
/


-- Pentru problema 2: definesc tipuri de date care mă ajută să rețin în ce orașe
-- s-a aflat un angajat, și pe ce perioadă de timp.

-- Definesc un tip de date care reprezintă o perioadă de zile
-- dintre două date calendaristicie.
CREATE TYPE t_interval AS OBJECT (data_start DATE, data_end DATE);
/

-- Creez un tip de date care reține în ce oraș s-a aflat un angajat,
-- și pentru ce perioadă.
CREATE TYPE t_stationare AS OBJECT (oras NVARCHAR2(30), durata t_interval);
/

-- Tabel imbricat care reține orașele în care a fost un angajat,
-- precum și perioadele în care s-a aflat acolo.
CREATE TYPE t_locatii_vizitate AS TABLE OF t_stationare;
/

ALTER TABLE employees
ADD locatii_vizitate t_locatii_vizitate
NESTED TABLE locatii_vizitate STORE AS employees_locatii_vizitate;
/


DECLARE
    v_interval t_interval;
    v_stationare t_stationare;
    v_locatii_vizitate t_locatii_vizitate;
BEGIN
    -- Pentru fiecare angajat, adaug orașul în care lucrează.
    -- Presupunem că s-a aflat acolo de la data angajării.
    FOR ang IN (
        SELECT employee_id, hire_date, city
        FROM employees
        INNER JOIN departments
        USING (department_id)
        INNER JOIN locations
        USING (location_id)
    )
    LOOP
        v_interval := t_interval(ang.hire_date, sysdate);
        v_stationare := t_stationare(ang.city, v_interval);
        v_locatii_vizitate := t_locatii_vizitate(v_stationare);

        UPDATE employees
        SET locatii_vizitate = v_locatii_vizitate
        WHERE employee_id = ang.employee_id;
    END LOOP;

    -- Dacă angajatul mai vizitează și alte orașe, putem actualiza lista.
END;
/
