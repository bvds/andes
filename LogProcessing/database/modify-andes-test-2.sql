-- Rebuild andes_test database
USE `andes_test`;
DROP TABLE IF EXISTS PROBLEM_ATTEMPT_TRANSACTION;
DROP TABLE IF EXISTS PROBLEM_ATTEMPT;

-- Mirrors Andes3 creation
\. create_CLASS_INFORMATION.sql
\. create_PROBLEM_ATTEMPT.sql
\. create_STEP_TRANSACTION.sql
\. create_STUDENT_STATE.sql
