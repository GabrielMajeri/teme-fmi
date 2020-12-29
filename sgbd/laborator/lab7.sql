---- Laboratorul 7: Tratarea excepțiilor ----

--- Exercițiul 1
DECLARE
    v_numar NUMBER;
    negative_input EXCEPTION;
BEGIN
    v_numar := &numar;

    IF v_numar < 0 THEN
        RAISE negative_input;
    END IF;

    DBMS_OUTPUT.PUT_LINE(SQRT(v_numar));
EXCEPTION
    WHEN negative_input THEN
        RAISE_APPLICATION_ERROR(
            -20000,
            'Numarul dat este invalid'
        );

END;
/

-- Alternativ:
DECLARE
    v_numar NUMBER;
BEGIN
    v_numar := &numar;

    DBMS_OUTPUT.PUT_LINE(SQRT(v_numar));
EXCEPTION
    WHEN value_error THEN
        RAISE_APPLICATION_ERROR(
            -20000,
            'Numarul dat este invalid'
        );
END;
/


--- Exercițiul 6
DECLARE
    v_nume departments.department_name%TYPE;
BEGIN
    BEGIN
        SELECT department_name INTO v_nume
        FROM departments
        INNER JOIN locations
        USING (location_id)
        WHERE city LIKE &oras;

        DBMS_OUTPUT.PUT_LINE('Nume departament: ' || v_nume);
    EXCEPTION
        WHEN no_data_found THEN
            raise_application_error(
                -20000,
                'Nu am gasit un departament cu locatia ceruta'
            );
    END;

    BEGIN
        SELECT department_name INTO v_nume
        FROM departments
        WHERE department_id = &id_departament;

        DBMS_OUTPUT.PUT_LINE('Nume departament: ' || v_nume);
    EXCEPTION
        WHEN no_data_found THEN
            raise_application_error(
                -20001,
                'Nu am gasit un departament cu ID-ul dat'
            );
    END;
END;
/
