CREATE TABLE `STUDENT_DATASET` (
  `datasetID` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `datasetname` varchar(250) NOT NULL,
  `modulename` varchar(45) NOT NULL,
  `groupname` varchar(45) NOT NULL,
  `problemname` varchar(45) NOT NULL,
  PRIMARY KEY (`datasetID`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1 COMMENT='A table for a dataset'