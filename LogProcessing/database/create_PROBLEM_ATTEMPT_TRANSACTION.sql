CREATE TABLE `PROBLEM_ATTEMPT_TRANSACTION` (
  `tID` int(10) unsigned NOT NULL auto_increment,
  `clientID` varchar(50) NOT NULL,
  `command` text,
  `initiatingParty` enum('client','server') NOT NULL,
  PRIMARY KEY  USING BTREE (`tID`,`clientID`),
  KEY `FK_transaction_problemstate` (`clientID`),
  CONSTRAINT `FK_transaction_problemstate` FOREIGN KEY (`clientID`) REFERENCES `PROBLEM_ATTEMPT` (`clientID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=1644 DEFAULT CHARSET=latin1 COMMENT='each attempt has a series of transactions associated with it'