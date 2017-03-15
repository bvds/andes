CREATE TABLE `REVIEWED_PROBLEMS` (
 `extra` int(10),
 `userName` varchar(50) NOT NULL,
 `tID` int(10) unsigned DEFAULT 0,
 `adminName` varchar(50) NOT NULL,
 `radioID` int(10) unsigned DEFAULT 0,
 `myComment` text,
 PRIMARY KEY(`extra`)
)ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='Base Table corresponding to the set of problems reviewed by the admin';

GRANT SELECT,INSERT,UPDATE ON REVIEWED_PROBLEMS TO 'open'@'localhost';
