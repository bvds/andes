-- We don't preserve history of meta-comments or checkboxes.
CREATE TABLE `REVIEWED_PROBLEMS` (
 `tID` int(10) unsigned NOT NULL,
 `userName` varchar(50) NOT NULL,
 `radioID` int(10) unsigned DEFAULT 0,
 `myComment` text,
 PRIMARY KEY (`tID`,`userName`),
 CONSTRAINT `FK_reviewed_tID` FOREIGN KEY (`tID`) REFERENCES `STEP_TRANSACTION` (`tID`) ON DELETE CASCADE ON UPDATE CASCADE
)ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='Base Table corresponding to the set of problems reviewed by the admin';

GRANT SELECT,INSERT,UPDATE ON REVIEWED_PROBLEMS TO 'open'@'localhost';
