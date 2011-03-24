CREATE TABLE `CLASS_INFORMATION` (
  `classSection` varchar(50) NOT NULL,
  `name` varchar(45) NULL,
  `school` varchar(45) NULL,
  `period` varchar(45) NULL,
  `description` varchar(250) NULL,
  `instructorName` varchar(50) NULL,
  `schoolyearInfo` varchar(50) NULL,
  `datasetID` int(10) unsigned NULL COMMENT 'If it exists, should match datasetID in STUDENT_DATASET',
  PRIMARY KEY  (`classSection`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1 COMMENT='A table for class information';
insert into CLASS_INFORMATION (classSection) values ("defaultSection");
