ALTER TABLE `STUDENT_STATE`
  MODIFY COLUMN `userName` varchar(50) NOT NULL DEFAULT '' COMMENT 'Empty string means that this is a section-wide model.  Otherwise, should match userName in PROBLEM_ATTEMPT';
ALTER TABLE `PROBLEM_ATTEMPT`
  MODIFY COLUMN `userName` varchar(50) NOT NULL COMMENT 'Needs to hold encrypted names.';
GRANT SELECT,INSERT,UPDATE ON CLASS_INFORMATION TO 'open'@'localhost';
GRANT USAGE ON PROBLEM_ATTEMPT TO 'open'@'localhost';
GRANT SELECT ON STEP_TRANSACTION TO 'open'@'localhost';
GRANT USAGE ON STUDENT_STATE TO 'open'@'localhost';

-- Add views for seeing users with consent.
\. create_CONSENT.sql
