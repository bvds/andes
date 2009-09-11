CREATE TABLE `PROBLEM_ATTEMPT_TRANSACTION` (
  `tID` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `clientID` varchar(50) NOT NULL,
  `command` text NOT NULL,
  `initiatingParty` enum('client','server') NOT NULL,
  PRIMARY KEY (`tID`,`clientID`) USING BTREE,
  KEY `FK_transaction_problemstate` (`clientID`),
  CONSTRAINT `FK_transaction_problemstate` FOREIGN KEY (`clientID`) REFERENCES `PROBLEM_ATTEMPT` (`clientID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='each attempt has a series of transactions associated with it'