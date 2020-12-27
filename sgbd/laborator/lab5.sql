---- Laboratorul 5: Pachete ----

-- (Re)creez o copie a tabelului `employees`:
DROP TABLE emp;
CREATE TABLE emp AS SELECT * FROM employees;
/

-- (Re)creez o copie a tabelului `departments`:
DROP TABLE dept;
CREATE TABLE dept AS SELECT * FROM departments;

-- (Re)creez o copie a tabelului `job_history`:
DROP TABLE job_hist;
CREATE TABLE job_hist AS SELECT * FROM job_history;

-- (Re)creez o secvență pentru ID-urile angajaților.
DROP SEQUENCE emp_seq;
CREATE SEQUENCE emp_seq
START WITH 1000
INCREMENT BY 1
NOCACHE NOCYCLE;
/

-- Salvez modificările asupra tabelelor.
COMMIT;
/

CREATE OR REPLACE PACKAGE pachet_companie
IS
    -- Găsește job-ul cu numele dat.
    FUNCTION gaseste_job(
        nume jobs.job_title%TYPE
    ) RETURN jobs.job_id%TYPE;

    -- Găsește angajatul cu numele și prenumele dat.
    FUNCTION gaseste_angajat(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE
    ) RETURN emp.employee_id%TYPE;

    -- Găsește departamentul cu numele dat.
    FUNCTION gaseste_departament(
        nume dept.department_name%TYPE
    ) RETURN dept.department_id%TYPE;

    -- Găsește salariul minim pentru departamentul și job-ul dat.
    FUNCTION gaseste_cel_mai_mic_salariu(
        id_dept dept.department_id%TYPE,
        id_job jobs.job_id%TYPE
    ) RETURN emp.salary%TYPE;

    -- Găsește comisionul minim (sau NULL, dacă nu există)
    -- pentru departamentul și job-ul dat.
    FUNCTION gaseste_cel_mai_mic_comision(
        id_dept dept.department_id%TYPE,
        id_job jobs.job_id%TYPE
    ) RETURN emp.commission_pct%TYPE;

    -- Adaugă un nou angajat.
    PROCEDURE adauga(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE,
        adresa_email emp.email%TYPE,
        telefon emp.phone_number%TYPE,
        prenume_manager emp.first_name%TYPE,
        nume_manager emp.last_name%TYPE,
        nume_departament dept.department_name%TYPE,
        nume_job jobs.job_title%TYPE
    );

    -- Mută un angajat în alt departament.
    PROCEDURE muta(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE,
        nume_departament dept.department_name%TYPE,
        nume_job jobs.job_title%TYPE,
        prenume_manager emp.first_name%TYPE,
        nume_manager emp.last_name%TYPE
    );
END;
/

CREATE OR REPLACE PACKAGE BODY pachet_companie
IS
    FUNCTION gaseste_job(
        nume jobs.job_title%TYPE
    ) RETURN jobs.job_id%TYPE
    IS
        v_id jobs.job_id%TYPE;
    BEGIN
        SELECT job_id INTO v_id
        FROM jobs
        WHERE job_title = nume;

        RETURN v_id;

    EXCEPTION
        WHEN no_data_found THEN
            raise_application_error(-20000, 'Nu am gasit jobul ' || nume);
    END;

    FUNCTION gaseste_angajat(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE
    ) RETURN emp.employee_id%TYPE
    IS
        v_id emp.employee_id%TYPE;
    BEGIN
        SELECT employee_id INTO v_id
        FROM emp
        WHERE first_name = prenume AND last_name = nume;

        RETURN v_id;

    EXCEPTION
        WHEN no_data_found THEN
            raise_application_error(-20000, 'Nu am gasit angajatul '
                    || prenume || ' ' || nume);
    END;

    FUNCTION gaseste_departament(
        nume dept.department_name%TYPE
    ) RETURN dept.department_id%TYPE
    IS
        v_id dept.department_id%TYPE;
    BEGIN
        SELECT department_id INTO v_id
        FROM dept
        WHERE department_name = nume;

        RETURN v_id;

    EXCEPTION
        WHEN no_data_found THEN
            raise_application_error(-20000, 'Nu am gasit departamentul ' || nume);
    END;

    FUNCTION gaseste_cel_mai_mic_salariu(
        id_dept dept.department_id%TYPE,
        id_job jobs.job_id%TYPE
    ) RETURN emp.salary%TYPE
    IS
        v_salariu emp.salary%TYPE;
    BEGIN
        SELECT MIN(salary) INTO v_salariu
        FROM emp
        WHERE department_id = id_dept AND job_id = id_job;

        IF v_salariu IS NULL THEN
            raise_application_error(-20000,
                'Nu exista angajati cu departamentul si jobul dat');
        END IF;

        RETURN v_salariu;
    END;

    FUNCTION gaseste_cel_mai_mic_comision(
        id_dept dept.department_id%TYPE,
        id_job jobs.job_id%TYPE
    ) RETURN emp.commission_pct%TYPE
    IS
        v_com emp.commission_pct%TYPE;
    BEGIN
        SELECT MIN(commission_pct) INTO v_com
        FROM emp
        WHERE department_id = id_dept AND job_id = id_job;

        RETURN v_com;
    END;


    PROCEDURE adauga(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE,
        adresa_email emp.email%TYPE,
        telefon emp.phone_number%TYPE,
        prenume_manager emp.first_name%TYPE,
        nume_manager emp.last_name%TYPE,
        nume_departament dept.department_name%TYPE,
        nume_job jobs.job_title%TYPE
    ) IS
        v_emp_id emp.employee_id%TYPE;
        v_dept_id dept.department_id%TYPE;
        v_job_id emp.job_id%TYPE;
        v_salariu emp.salary%TYPE;
    BEGIN
        v_emp_id := emp_seq.NEXTVAL;
        v_dept_id := gaseste_departament(nume_departament);
        v_job_id := gaseste_job(nume_job);
        v_salariu := gaseste_cel_mai_mic_salariu(v_dept_id, v_job_id);

        INSERT INTO emp (
            employee_id, first_name, last_name,
            email, phone_number, hire_date,
            job_id,
            salary, commission_pct,
            manager_id,
            department_id
        )
        VALUES (
            v_emp_id, prenume, nume,
            adresa_email, telefon, SYSDATE,
            v_job_id,
            v_salariu, 0,
            gaseste_angajat(prenume_manager, nume_manager),
            v_dept_id
        );
    END;

    PROCEDURE muta(
        prenume emp.first_name%TYPE,
        nume emp.last_name%TYPE,
        nume_departament dept.department_name%TYPE,
        nume_job jobs.job_title%TYPE,
        prenume_manager emp.first_name%TYPE,
        nume_manager emp.last_name%TYPE
    ) IS
        v_ang emp%ROWTYPE;
        v_manager_nou emp.employee_id%TYPE;
        v_dept_nou dept.department_id%TYPE;
        v_job_nou emp.job_id%TYPE;
        v_salariu_nou emp.salary%TYPE;
        v_com_nou emp.commission_pct%TYPE;
    BEGIN
        SELECT * INTO v_ang
        FROM emp
        WHERE employee_id = gaseste_angajat(prenume, nume);

        INSERT INTO job_hist (
            employee_id,
            start_date, end_date,
            job_id, department_id
        )
        VALUES (
            v_ang.employee_id,
            v_ang.hire_date, SYSDATE,
            v_ang.job_id, v_ang.department_id
        );

        v_dept_nou := gaseste_departament(nume_departament);
        v_job_nou := gaseste_job(nume_job);
        v_manager_nou := gaseste_angajat(prenume_manager, nume_manager);
        v_salariu_nou := gaseste_cel_mai_mic_salariu(v_dept_nou, v_job_nou);
        v_com_nou := gaseste_cel_mai_mic_comision(v_dept_nou, v_job_nou);

        UPDATE emp
        SET
            department_id = v_dept_nou,
            job_id = v_job_nou,
            manager_id = v_manager_nou,
            salary = v_salariu_nou,
            commission_pct = v_com_nou,
            hire_date = SYSDATE
        WHERE employee_id = v_ang.employee_id;
    END;
END;
/

-- Testez funcțiile din pachet.
BEGIN
    pachet_companie.adauga(
        'John', 'Test',
        'test@example.com', '1234',
        'Steven', 'King',
        'Sales',
        'Sales Manager'
    );

    pachet_companie.muta(
        'John', 'Test',
        'Finance',
        'Finance Manager',
        'Lex', 'De Haan'
    );
END;
/

-- Afișez angajatul care a fost adăugat.
SELECT *
FROM emp
WHERE employee_id >= 1000;

-- Șterg angajații creați.
DELETE FROM emp
WHERE employee_id >= 1000;
