CREATE TABLE `CLASS_INFORMATION` (
  `classID` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(45) NOT NULL,
  `school` varchar(45) NOT NULL,
  `period` varchar(45) NOT NULL,
  `description` varchar(250) NOT NULL,
  `instructorName` varchar(50) NOT NULL,
  `schoolyearInfo` varchar(50) NOT NULL,
  `datasetID` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`classID`),
  KEY `FK_classinformation_dataset` (`datasetID`),
  CONSTRAINT `FK_classinformation_dataset` FOREIGN KEY (`datasetID`) REFERENCES `STUDENT_DATASET` (`datasetID`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1 COMMENT='A table for class information'