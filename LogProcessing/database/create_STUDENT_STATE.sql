CREATE TABLE IF NOT EXISTS `STUDENT_STATE` (
  `userSection` varchar(50) NOT NULL COMMENT 'Should match section in CLASS_INFORMATION.',
  `userName` varchar(20) NULL COMMENT 'NULL means that this is a section-wide model.  Otherwise, should match userName in PROBLEM_ATTEMPT',
  `model` varchar(20) NOT NULL DEFAULT 'default' COMMENT '"default" is default student model; "client"/"server" are client/help system customizations.',
  `property` varchar(50) NOT NULL COMMENT 'For a student model, this is the name of the knowledge component.',
  `tID` int(10) unsigned NOT NULL,
  `value` text NULL COMMENT 'Description of current state as json object or lisp sexpr.  NULL is equivalent to removing a property from the table.',
  UNIQUE (`userSection`,`userName`,`model`,`property`,`tID`),
  KEY `FK_model_class` (`userSection`),
  CONSTRAINT `FK_model_class` FOREIGN KEY (`userSection`) REFERENCES `CLASS_INFORMATION` (`classSection`) ON DELETE CASCADE ON UPDATE CASCADE,
  KEY `FK_step_state` (`tID`),
  CONSTRAINT `FK_step_state` FOREIGN KEY (`tID`) REFERENCES `STEP_TRANSACTION` (`tID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=1644 DEFAULT CHARSET=latin1 COMMENT='Changes to state.  Changes within a session may be consolidated.  Changes in a session may use any tID within that session timeframe.  Entries that do not refect a change in value may be included.';