
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
CREATE DATABASE `andes` /*!40100 DEFAULT CHARACTER SET latin1 */;
USE `andes`;
-- Creates the tables NOTE: ORDER OF CREATION MATTERS DUE TO FOREIGN KEY DEPENDENCIES! (ALWAYS MAKE SURE YOU CREATE A TABLE BEFORE YOU CREATE ANOTHER THAT REFERENCES IT)
\. create_STUDENT_DATASET.sql
\. create_CLASS_INFORMATION.sql
\. create_PROBLEM_ATTEMPT.sql
\. create_PROBLEM_ATTEMPT_TRANSACTION.sql
\. create_REVIEWED_PROBLEMS.sql

-- Insert initial classes into database

insert into STUDENT_DATASET values (1,"Watchung Hills Regional High School Honors Physics 2008-2009","Statics","S*","S2E");
insert into STUDENT_DATASET values (2,"dummy dataset","dummy module","dummy group","dummy problem");

insert into CLASS_INFORMATION values (1,"Physics H (430)","Watchung Hills Regional High School",3,"Introductory physics course for 11th and 12th grades preparing students for the physics SAT II exam.","Brian Brown","2008-2009",1);
insert into CLASS_INFORMATION values (2,"dummy class","dummy school",0,"a dummy class used for lack of knowledge","dumb instructor","dumb year",2);

-- Some mysql installations have file loading disabled.
-- This is the case for CMU/OLI

-- LOAD DATA LOCAL INFILE 'student_dataset.csv' INTO TABLE STUDENT_DATASET FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' (datasetID, datasetname, modulename, groupname, problemname);
-- LOAD DATA LOCAL INFILE 'classinformation.csv' INTO TABLE CLASS_INFORMATION FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n' (classID, name, school, period, description, instructorName, schoolyearInfo, datasetID);

-- Create a database for load testing
CREATE DATABASE `andes_test` /*!40100 DEFAULT CHARACTER SET latin1 */;
USE `andes_test`;
-- Create the tables.  The test tables have a simpler structure than 'andes'.

\. create_test_PROBLEM_ATTEMPT.sql
\. create_PROBLEM_ATTEMPT_TRANSACTION.sql