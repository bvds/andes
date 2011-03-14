CREATE TABLE `PROBLEM_ATTEMPT` (
  `userName` varchar(20) NOT NULL,
  `sessionID` varchar(45) NOT NULL,
  `startTime` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `clientID` varchar(50) NOT NULL default 'fill in clientID',
  `classinformationID` int(10) unsigned NOT NULL,
  `userProblem` varchar(50) default NULL COMMENT 'The problem the user asks for when communicating with server. May or may not exist.',
  `userSection` varchar(50) default NULL COMMENT 'The section the user is enrolled in. may or may not exist. ',
  `extra` int(10) DEFAULT 0,
  PRIMARY KEY  (`clientID`)) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='the base table corresponding to problem state at any given t'
