-- Exercițiul 1

-- Vreau ca atunci când șterg o prezentare, să setez să fie NULL coloana
-- `cod_prezentare` a vestimentațiilor care erau în acea prezentare.
CREATE OR REPLACE TRIGGER on_delete_prezentare
FOR DELETE ON prezentare
COMPOUND TRIGGER
    -- Tabel în care salvez codurile prezentărilor șterse
    TYPE t_coduri_prezentari IS TABLE OF prezentare.cod_pr%TYPE;
    v_sterse t_coduri_prezentari := t_coduri_prezentari();

    BEFORE EACH ROW IS
    BEGIN
        -- Salvez codurile prezentărilor șterse
        v_sterse.EXTEND;
        v_sterse(v_sterse.LAST) := :OLD.cod_pr;
    END BEFORE EACH ROW;

    AFTER STATEMENT IS
    BEGIN
        -- Realizez actualizarea cu un FORALL
        FORALL i IN v_sterse.FIRST..v_sterse.LAST
            UPDATE vestimentatie
            SET cod_prezentare = NULL
            WHERE cod_prezentare = v_sterse(i);
    END AFTER STATEMENT;
END on_delete_prezentare;
/

-- Verific cum se numește foreign key-ul existent
SELECT constraint_name
FROM user_constraints
WHERE (table_name = 'VESTIMENTATIE') AND (constraint_type = 'R');

-- Îl elimin
ALTER TABLE vestimentatie
DROP CONSTRAINT SYS_C00506751;

-- Testez trigger-ul
DELETE FROM prezentare
WHERE cod_pr IN 4504;

ROLLBACK;



-- Exercițiul 2
CREATE TABLE num_vestimentatii (
    cod_pr NUMBER,
    numar NUMBER
);

CREATE OR REPLACE TRIGGER update_nr_vestimentatii
FOR UPDATE OF cod_prezentare OR DELETE
ON vestimentatie
COMPOUND TRIGGER
    BEFORE STATEMENT IS
    BEGIN
        -- Actualizez tabelul cu numărul de vestimentații per prezentare
        DELETE FROM num_vestimentatii;

        INSERT INTO num_vestimentatii (cod_pr, numar)
        SELECT cod_prezentare, COUNT(1)
        FROM vestimentatie
        WHERE cod_prezentare IS NOT NULL
        GROUP BY cod_prezentare;
    END BEFORE STATEMENT;

    BEFORE EACH ROW IS
        v_numar NUMBER;
    BEGIN
        -- Văd câte vestimentații am în această prezentare înainte de modificare
        SELECT numar INTO v_numar
        FROM num_vestimentatii
        WHERE cod_pr = :OLD.cod_prezentare;

        IF UPDATING THEN
            -- Dacă se face o actualizare, verific dacă diferă noul cod
            IF :NEW.cod_prezentare != :OLD.cod_prezentare THEN
                v_numar := v_numar - 1;

                -- Actualizez numărătoarea pentru noua prezentare setată
                UPDATE num_vestimentatii
                SET numar = numar + 1
                WHERE cod_pr = :NEW.cod_prezentare;
            END IF;
        ELSE
            v_numar := v_numar - 1;
        END IF;

        IF v_numar < 5 THEN
            raise_application_error(-20000,
                'Prezentarea ar avea mai putin de 5 vestimentatii');
        END IF;

        -- Actualizez nr. de vestimentații pentru această prezentare
        UPDATE num_vestimentatii
        SET numar = v_numar
        WHERE cod_pr = :OLD.cod_prezentare;
    END BEFORE EACH ROW;
END;
/

-- Testez trigger-ul
DELETE FROM vestimentatie
WHERE cod_vestimentatie IN (6300, 6312);

ROLLBACK;
