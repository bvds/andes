ALTER TABLE `PROBLEM_ATTEMPT` DROP FOREIGN KEY FK_problemstate_classinformation;
ALTER TABLE `CLASS_INFORMATION` DROP PRIMARY KEY,
  CHANGE COLUMN classID classSection VARCHAR(50) NOT NULL PRIMARY KEY,
  DROP FOREIGN KEY FK_classinformation_dataset;
ALTER TABLE `CLASS_INFORMATION` 
  MODIFY COLUMN `name` varchar(45) NULL,
  MODIFY COLUMN `school` varchar(45) NULL,
  MODIFY COLUMN `period` varchar(45) NULL,
  MODIFY COLUMN `description` varchar(250) NULL,
  MODIFY COLUMN `instructorName` varchar(50) NULL,
  MODIFY COLUMN `schoolyearInfo` varchar(50) NULL,
  MODIFY COLUMN `datasetID` int(10) unsigned NULL COMMENT 'If it exists, should match datasetID in STUDENT_DATASET';
REPLACE INTO CLASS_INFORMATION (classSection) values ("defaultSection");
REPLACE INTO CLASS_INFORMATION (classSection) SELECT DISTINCT userSection FROM
  PROBLEM_ATTEMPT WHERE userSection is not null;
