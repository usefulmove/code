
CREATE TABLE survey_one
(
  name varchar(32),
  dept varchar(32),
  kd1 int,
  kd2 int,
  kd3 int,
  kd4 int,
  kd5 int
);

INSERT INTO survey_one(
  name,
  dept,
  kd1,
  kd2,
  kd3,
  kd4,
  kd5
) VALUES
  ('Alex', 'MFG', 1, 2, 3, 4, 5),
  ('Ron', 'ENG', 4, 4, 4, 3, 5),
  ('Tom', 'ENG', 5, 2, 3, 4, 5),
  ('Sonny', 'HR', 4, 5, 3, 4, 5),
  ('Danielle', 'EXEC', 3, 5, 3, 3, 5),
  ('Dave', 'EXEC', 4, 4, 4, 3, 5),
  ('Tolover', 'LAB', 4, 4, 4, 4, 4),
  ('Ronald', 'LAB', 5, 4, 3, 4, 4),
  ('Cora', 'LAB', 5, 4, 3, 5, 5);

SELECT * FROM survey_one;


---


SELECT
  department AS dept,
  AVG(question1) AS safety,
  AVG(question2) AS depend,
  AVG(question3) AS clarity,
  AVG(question4) AS meaning,
  AVG(question5) AS impact
FROM
  employee_survey s LEFT JOIN employee e
    ON s.employeeID = e.employeeID
GROUP BY
  e.department
;
