SET VERIFY OFF;

--- Exerciții
-- 1

-- Rezolvare cu cicluri cursor
DECLARE
    v_num_ang NUMBER;
    v_sal_lunar NUMBER;
    v_sal_mediu NUMBER;
    v_contor_ang NUMBER;

    v_nr_ang NUMBER;
    v_sal_total NUMBER;
    v_sal_total_med NUMBER;
BEGIN
    -- Parcurg toate job-urile cu un ciclu cursor
    FOR i IN (SELECT job_id, job_title FROM jobs)
    LOOP
        DBMS_OUTPUT.PUT_LINE('Jobul ' || i.job_title);

        -- Extrag câteva statistici despre job-ul curent
        SELECT COUNT(1), SUM(salary), AVG(salary)
        INTO v_num_ang, v_sal_lunar, v_sal_mediu
        FROM employees
        WHERE job_id = i.job_id;

        -- Afișez statisticile
        IF v_num_ang = 0 THEN
            DBMS_OUTPUT.PUT_LINE('Nu avem angajati cu acest job');
        ELSE
            DBMS_OUTPUT.PUT_LINE('- Nr. angajati: ' || v_num_ang);
            DBMS_OUTPUT.PUT_LINE('- Salariu lunar total: ' || v_sal_lunar);
            DBMS_OUTPUT.PUT_LINE('- Salariu mediu: ' || v_sal_mediu);
        END IF;

        -- Afișez și angajații
        v_contor_ang := 1;
        FOR j IN (SELECT first_name, last_name, salary
                  FROM employees
                  WHERE job_id = i.job_id)
        LOOP
            DBMS_OUTPUT.PUT_LINE(
                '    Angajatul ' || v_contor_ang || ': '
                || j.first_name || ' ' || j.last_name
                || ' cu salariul ' || j.salary
            );
        
            -- Contorizez al câtelea angajat este
            v_contor_ang := v_contor_ang + 1;
        END LOOP;

        DBMS_OUTPUT.PUT_LINE('');
    END LOOP;

    -- Calculez statisticile pentru toți angajații
    SELECT
        COUNT(1),
        SUM(salary),
        AVG(salary)
    INTO
        v_nr_ang,
        v_sal_total,
        v_sal_total_med
    FROM employees;

    DBMS_OUTPUT.PUT_LINE('Nr. total ang.: ' || v_nr_ang);
    DBMS_OUTPUT.PUT_LINE('Sal. total ang.: ' || v_sal_total);
    DBMS_OUTPUT.PUT_LINE('Sal. mediu ang.: ' || v_sal_total_med);
END;
/


-- 3
DECLARE
    v_suma_totala NUMBER;
    v_suma_ang NUMBER;
BEGIN
    -- Calculez suma totală alocată lunar pentru salarii și comisioane.
    SELECT SUM(salary) + SUM(salary * NVL(commission_pct, 0))
    INTO v_suma_totala
    FROM employees;
    DBMS_OUTPUT.PUT_LINE('Suma totala alocata lunar: ' || v_suma_totala);

    -- Parcurg toate job-urile cu un ciclu cursor
    FOR i IN (SELECT job_id, job_title FROM jobs)
    LOOP
        DBMS_OUTPUT.PUT_LINE('Jobul ' || i.job_title);

        -- Afișez angajații
        FOR j IN (SELECT first_name, last_name, salary, commission_pct
                  FROM employees
                  WHERE job_id = i.job_id)
        LOOP
            -- Calculez suma primită lunar de angajat
            v_suma_ang := j.salary * (1 + NVL(j.commission_pct, 0));
            DBMS_OUTPUT.PUT_LINE(
                '    Angajatul '
                || j.first_name || ' ' || j.last_name
                || ' castiga lunar ' ||
                ROUND(v_suma_ang / v_suma_totala * 100, 4)
                || ' la suta'
            );
        END LOOP;

        DBMS_OUTPUT.PUT_LINE('');
    END LOOP;
END;
/


-- 4
DECLARE
    TYPE t_sal_ang IS RECORD (
        id NUMBER,
        name VARCHAR2(50),
        salary NUMBER
    );
    TYPE t_vec_sal_ang IS VARRAY(5) OF t_sal_ang;
    v_top_ang t_vec_sal_ang;
BEGIN
    FOR i IN (SELECT job_id, job_title FROM jobs)
    LOOP
        DBMS_OUTPUT.PUT_LINE('Jobul ' || i.job_title);

        -- Aleg primii 5 angajați, după salariu
        SELECT *
        BULK COLLECT INTO v_top_ang
        FROM (
            SELECT
                employee_id,
                first_name || ' ' || last_name,
                salary
            FROM employees
            WHERE job_id = i.job_id
            ORDER BY salary DESC
        )
        WHERE rownum <= 5;

        IF v_top_ang.COUNT < 5 THEN
            DBMS_OUTPUT.PUT_LINE('Are mai putin de 5 angajati');
        END IF;

        -- Dacă am măcar un angajat
        IF v_top_ang.COUNT > 0 THEN
            FOR i IN v_top_ang.FIRST..v_top_ang.LAST LOOP
                DBMS_OUTPUT.PUT_LINE(v_top_ang(i).name);
            END LOOP;
        END IF;

        DBMS_OUTPUT.PUT_LINE('');
    END LOOP;
END;
/


-- 5
DECLARE
    -- Rețin numărul de salarii distincte afișate deja
    v_nr_ang NUMBER;
    -- Rețin valoarea ultimului salariu afișat
    v_ultimul_sal NUMBER;
BEGIN
    FOR i IN (SELECT job_id, job_title FROM jobs)
    LOOP
        DBMS_OUTPUT.PUT_LINE('Jobul ' || i.job_title);
        
        v_nr_ang := 0;
        v_ultimul_sal := -1;

        -- Parcurg toți angajații, după salariu
        FOR e IN (
                SELECT
                    employee_id,
                    first_name || ' ' || last_name AS name,
                    salary
                FROM employees
                WHERE job_id = i.job_id
                ORDER BY salary DESC
            )
        LOOP
            -- Mă opresc când am afișat angajații cu primele 5 salarii
            EXIT WHEN v_nr_ang >= 5;
            
            -- Dacă e un alt salariu, atunci e pe următorul loc în top
            IF e.salary != v_ultimul_sal THEN
                v_nr_ang := v_nr_ang + 1;          
            END IF;
            
            DBMS_OUTPUT.PUT_LINE('Locul ' || v_nr_ang || ': '
                || e.name || ' cu ' || e.salary);
                
            v_ultimul_sal := e.salary;
        END LOOP;

        DBMS_OUTPUT.PUT_LINE('');
    END LOOP;
END;
/
