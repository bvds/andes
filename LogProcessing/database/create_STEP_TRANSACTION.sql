CREATE TABLE `STEP_TRANSACTION` (
  `tID` int(10) unsigned NOT NULL auto_increment,
  `clientID` varchar(50) NOT NULL,
  `client` text NULL,
  `server` text NULL,
  PRIMARY KEY  USING BTREE (`tID`,`clientID`),
  KEY `FK_step_problem` (`clientID`),
  CONSTRAINT `FK_step_problem` FOREIGN KEY (`clientID`) REFERENCES `PROBLEM_ATTEMPT` (`clientID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=1644 DEFAULT CHARSET=latin1 COMMENT='each attempt has a series of transactions associated with it'