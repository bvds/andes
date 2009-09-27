CREATE TABLE `STUDENT_DATASET` (
  `datasetID` int(10) unsigned NOT NULL auto_increment,
  `datasetname` varchar(250) NOT NULL,
  `modulename` varchar(45) NOT NULL,
  `groupname` varchar(45) NOT NULL,
  `problemname` varchar(45) NOT NULL,
  PRIMARY KEY  (`datasetID`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=latin1 COMMENT='A table for a dataset'