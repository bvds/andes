/* AndesDatabaseCreationSQL.sql
   This is a set of batch SQL commands created by Nicholas Vaidyanathan to simplify 
   the process of installing the Andes 3 logging DB Schema. To use this script, 
   run mysql -u root and enter in the password from the shell environment, then once
   mysql is started issue the command:
	 \. AndesDatabaseCreationSQL.sql
   
  This is a lazy, barebones script that will just create the andes database and all associated tables. It will then 
  attempt to populate them with data based upon CSV files stored in a local directory, in the present case, mine.
  In the future, this is a good candidate to be converted into a shell script, with command line arguments for the user directory
*/  
-- Creates the database
DROP DATABASE IF EXISTS `andes`;
CREATE DATABASE `andes` /*!40100 DEFAULT CHARACTER SET latin1 */;
USE `andes`;
-- Creates the tables NOTE: ORDER OF CREATION MATTERS DUE TO FOREIGN KEY DEPENDENCIES! (ALWAYS MAKE SURE YOU CREATE A TABLE BEFORE YOU CREATE ANOTHER THAT REFERENCES IT)
DROP TABLE IF EXISTS `andes`.`STUDENT_DATASET`;
\. create_STUDENT_DATASET.sql

DROP TABLE IF EXISTS `andes`.`CLASS_INFORMATION`;
\. create_CLASS_INFORMATION.sql

DROP TABLE IF EXISTS `andes`.`PROBLEM_ATTEMPT`;
\. create_PROBLEM_ATTEMPT.sql

DROP TABLE IF EXISTS `andes`.`PROBLEM_ATTEMPT_TRANSACTION`;
\. create_PROBLEM_ATTEMPT_TRANSACTION.sql

-- USE THE CREATED DATABASE
USE andes;

-- Attempts to populate with data. For simplicity, hardcoded to my user directory. MOST LIKELY AREA TO CHANGE
 LOAD DATA LOCAL INFILE 'student_dataset.csv' INTO TABLE STUDENT_DATASET FIELDS TERMINATED BY ',' LINES TERMINATED BY 
'\n' (datasetID, datasetname, modulename, groupname, problemname);
 LOAD DATA LOCAL INFILE 'classinformation.csv' INTO TABLE CLASS_INFORMATION FIELDS TERMINATED BY ',' LINES TERMINATED BY 
'\n' (classID, name, school, period, description, instructorName, schoolyearInfo, datasetID);
LOAD DATA LOCAL INFILE 'problem_attempt.csv' INTO TABLE PROBLEM_ATTEMPT FIELDS TERMINATED BY ',' LINES TERMINATED BY 
'\n' (userName, sessionID, startTime, clientID, classinformationID);
-- LOAD DATA LOCAL INFILE '/home/nvaidyan/problem_attempt_transaction.csv' INTO TABLE PROBLEM_ATTEMPT_TRANSACTION FIELDS TERMINATED BY ',' LINES TERMINATED BY 
-- '\n' (tID, clientID, command, initiatingParty);

