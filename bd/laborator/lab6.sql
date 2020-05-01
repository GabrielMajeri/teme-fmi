1.

SELECT DISTINCT t1.employee_id
FROM works_on t1
WHERE NOT EXISTS (
    SELECT 1
    FROM projects p
    WHERE (
        ('01-JAN-2006' <= start_date) AND
        (start_date < '01-JUL-2006')
    )
    AND NOT EXISTS (
        SELECT 1
        FROM works_on t2
        WHERE t2.project_id = p.project_id
        AND t2.employee_id = t1.employee_id
    )
);


2.

WITH employees_with_two_jobs AS (
    SELECT employee_id
    FROM job_history
    GROUP BY employee_id
    HAVING COUNT(job_id) >= 2
)
SELECT p.project_id
FROM projects p
WHERE NOT EXISTS (
    SELECT 1
    FROM works_on t1
    INNER JOIN employees_with_two_jobs e2j
    ON (t1.employee_id = e2j.employee_id)
    WHERE NOT EXISTS (
        SELECT 1
        FROM works_on t2
        WHERE (
            (t2.employee_id = t1.employee_id) AND
            (t2.project_id = p.project_id)
        )
    )
);


3.

WITH num_previous_jobs AS (
    SELECT employee_id, COUNT(1) AS num_jobs
    FROM job_history
    GROUP BY employee_id
)
SELECT employee_id
FROM employees
INNER JOIN num_previous_jobs
USING (employee_id)
WHERE num_jobs >= 2;


4.

SELECT country_id, COUNT(1) AS num_employees
FROM employees
INNER JOIN departments
USING (department_id)
INNER JOIN locations
USING (location_id)
GROUP BY country_id;


5.

WITH nu_la_termen AS (
    SELECT project_id
    FROM projects
    WHERE delivery_date > deadline
)
SELECT employee_id, first_name, last_name
FROM employees e
WHERE 2 <= (
    SELECT COUNT(project_id)
    FROM works_on
    INNER JOIN nu_la_termen
    USING (project_id)
    WHERE employee_id = e.employee_id
);


6.

SELECT employee_id, first_name, last_name, project_id, project_name
FROM employees
LEFT OUTER JOIN works_on
USING (employee_id)
LEFT OUTER JOIN projects
USING (project_id);


7.

WITH project_managers AS (
    SELECT DISTINCT project_manager AS employee_id
    FROM projects
)
SELECT first_name, last_name, department_id
FROM employees
WHERE department_id IN (
    SELECT DISTINCT department_id
    FROM departments
    INNER JOIN employees
    USING (department_id)
    INNER JOIN project_managers
    USING (employee_id)
);


8.

WITH project_managers AS (
    SELECT DISTINCT project_manager AS employee_id
    FROM projects
)
SELECT first_name, last_name, department_id
FROM employees
WHERE department_id NOT IN (
    SELECT DISTINCT department_id
    FROM departments
    INNER JOIN employees
    USING (department_id)
    INNER JOIN project_managers
    USING (employee_id)
);


9.

SELECT department_id
FROM employees
GROUP BY department_id
HAVING AVG(salary) > &p;


10.

WITH project_managers AS (
    SELECT
        project_manager AS employee_id,
        COUNT(1) AS project_count
    FROM projects
    GROUP BY project_manager
)
SELECT first_name, last_name, salary, project_count
FROM employees
INNER JOIN project_managers
USING (employee_id)
WHERE project_count >= 2;


11.

SELECT DISTINCT employee_id
FROM works_on w1
WHERE NOT EXISTS (
    SELECT 1
    FROM projects p
    WHERE project_manager = 102 AND
    NOT EXISTS (
        SELECT 1
        FROM works_on w2
        WHERE w1.employee_id = w2.employee_id
            AND p.project_id = w2.project_id
    )
);


12.

a)

WITH employee_200_projects AS (
    SELECT project_id
    FROM works_on
    WHERE employee_id = 200
)
SELECT DISTINCT last_name
FROM employees e
WHERE NOT EXISTS (
    SELECT 1
    FROM employee_200_projects
    WHERE project_id NOT IN (
        SELECT project_id
        FROM works_on
        WHERE employee_id = e.employee_id
    )
);

b)

WITH employee_200_projects AS (
    SELECT project_id
    FROM works_on
    WHERE employee_id = 200
)
SELECT DISTINCT e.last_name
FROM employees e
WHERE NOT EXISTS (
    SELECT project_id
    FROM works_on
    WHERE (employee_id = e.employee_id)
        AND project_id NOT IN (
            SELECT *
            FROM employee_200_projects
        )
);


13.

WITH employee_200_projects AS (
    SELECT project_id
    FROM works_on
    WHERE employee_id = 200
)
SELECT e.employee_id, e.last_name
FROM employees e
WHERE NOT EXISTS (
    (
        SELECT project_id
        FROM works_on
        WHERE employee_id = e.employee_id
    )
    MINUS
    (
        SELECT project_id
        FROM employee_200_projects
    )
)
AND NOT EXISTS (
    (
        SELECT project_id
        FROM employee_200_projects
    )
    MINUS
    (
        SELECT project_id
        FROM works_on
        WHERE employee_id = e.employee_id
    )
);


14.

SELECT *
FROM job_grades;

SELECT last_name, first_name, salary, grade_level
FROM employees
INNER JOIN job_grades
ON ((lowest_sal <= salary) AND (salary <= highest_sal));


15.

SELECT employee_id, last_name, salary, department_id
FROM employees
WHERE employee_id = &cod_angajat;


16.

SELECT last_name, department_id, salary
FROM employees
WHERE job_id = &job_ales;


17.

SELECT last_name, department_id, salary
FROM employees
WHERE hire_date >= &data_angajare;


18.

SELECT &&coloana
FROM &&tabel
WHERE &&conditie
ORDER BY &coloana;


19.

ACCEPT data1 PROMPT "data start = ";
ACCEPT data2 PROMPT "data sfarsit = ";
SELECT last_name || ' ' || job_id AS Angajati, hire_date
FROM employees
WHERE (&data1 <= hire_date) AND (hire_date <= &data2);


20.

SELECT last_name, job_id, salary, department_name
FROM employees
INNER JOIN departments
USING (department_id)
INNER JOIN locations
USING (location_id)
WHERE city LIKE &locatie;


21.

a)

ACCEPT data1 PROMPT "data 1 = ";
ACCEPT data2 PROMPT "data 2 = ";
SELECT (TO_DATE(&data2) - TO_DATE(&data1)) AS diff
FROM dual;


b)

WITH num_weeks AS (
    SELECT (TO_DATE(&data2) - TO_DATE(&data1))/7 AS num_weeks
    FROM dual
)
SELECT 5 * num_weeks AS working_days
FROM num_weeks;
