/*
      Function to determine if student has given consent.
      If result does not exist for student, look for any
      sectional defaults.

      Should match function check-informed-consent in Help/sessions.cl

      select * from STUDENT_STATE where property regexp 'consent';
      SELECT consent('bvds','54321');
      SELECT DISTINCT userName,userSection FROM PROBLEM_ATTEMPT WHERE consent(userName,userSection);
*/
DROP FUNCTION IF EXISTS consent;
DELIMITER $$
CREATE FUNCTION consent(user VARCHAR(50),section VARCHAR(50))
  RETURNS BOOLEAN
  READS SQL DATA 
  BEGIN
     DECLARE agree TEXT;
     DECLARE CONTINUE HANDLER FOR NOT FOUND BEGIN END;
     SELECT value INTO agree FROM STUDENT_STATE WHERE
       model='client' AND property='informed-consent' AND
       userName=user AND userSection=section 
       ORDER BY tID DESC LIMIT 1;
     IF agree IS NULL THEN
       SELECT value INTO agree FROM STUDENT_STATE WHERE
         userName='' AND userSection=section AND
         model='client' AND property='informed-consent'
         ORDER BY tID DESC LIMIT 1;
     END IF;
     RETURN agree REGEXP '^"?(agree|external):';
  END $$
DROP FUNCTION IF EXISTS crypt $$
CREATE FUNCTION crypt(user VARCHAR(50))
  RETURNS VARCHAR(50)
  DETERMINISTIC
  BEGIN
  IF user REGEXP '^(bvds|x:)' THEN
    RETURN user;
  ELSEIF user REGEXP '^md5:[0-9a-f]*$' THEN
    RETURN user;
  ELSE
    RETURN CONCAT('md5:',md5(user));
  END IF;
  END $$
DELIMITER ;
GRANT EXECUTE, CREATE ROUTINE ON * TO 'open'@'localhost';

/*
   select Distinct userName,userSection from OPEN_PROBLEM_ATTEMPT; 
*/
DROP VIEW IF EXISTS `OPEN_PROBLEM_ATTEMPT`;
CREATE VIEW `OPEN_PROBLEM_ATTEMPT` 
  (userName,startTime,clientID,userProblem,userSection,extra) AS SELECT 
  crypt(userName),startTime,clientID,userProblem,userSection,extra 
  FROM `PROBLEM_ATTEMPT` WHERE consent(userName,userSection);
DROP VIEW IF EXISTS `OPEN_STUDENT_STATE`;
CREATE VIEW `OPEN_STUDENT_STATE` 
  (userName,userSection,model,property,tID,value) AS SELECT 
  crypt(userName),userSection,model,property,tID,value 
  FROM `STUDENT_STATE` WHERE consent(userName,userSection);

/*
   Adjust permissions for user 'open' so that they
   can only read and only have access to open views.

Start over:
REVOKE ALL PRIVILEGES, GRANT OPTION FROM 'open'@'localhost';

Do by hand:
GRANT SELECT ON PROBLEM_ATTEMPT_TRANSACTION TO 'open'@'localhost';

*/
GRANT SELECT ON OPEN_PROBLEM_ATTEMPT TO 'open'@'localhost';
GRANT SELECT ON OPEN_STUDENT_STATE  TO 'open'@'localhost';
