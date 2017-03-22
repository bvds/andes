-- Connect a copied solution to its parent.
CREATE TABLE `SOLUTION_COPY` (
 `clientID` varchar(50) NOT NULL,
 `tID` int(10) unsigned NOT NULL,
 PRIMARY KEY (`clientID`,`tID`),
 CONSTRAINT `FK_clientID` FOREIGN KEY (`clientID`) REFERENCES `PROBLEM_ATTEMPT` (`clientID`) ON DELETE CASCADE ON UPDATE CASCADE,
 CONSTRAINT `FK_tID` FOREIGN KEY (`tID`) REFERENCES `STEP_TRANSACTION` (`tID`) ON DELETE CASCADE ON UPDATE CASCADE
)ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='Connect a copied solution to its parent solution step.';

GRANT SELECT,INSERT,UPDATE ON `SOLUTION_COPY` TO 'open'@'localhost';
