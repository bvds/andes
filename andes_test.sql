-- MySQL dump 10.11
--
-- Host: localhost    Database: andes_test
-- ------------------------------------------------------
-- Server version	5.0.77-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `CLASS_INFORMATION`
--

DROP TABLE IF EXISTS `CLASS_INFORMATION`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `CLASS_INFORMATION` (
  `classSection` varchar(50) NOT NULL,
  `name` varchar(45) default NULL,
  `school` varchar(45) default NULL,
  `period` varchar(45) default NULL,
  `description` varchar(250) default NULL,
  `instructorName` varchar(50) default NULL,
  `schoolyearInfo` varchar(50) default NULL,
  `datasetID` int(10) unsigned default NULL COMMENT 'If it exists, should match datasetID in STUDENT_DATASET',
  PRIMARY KEY  (`classSection`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='A table for class information';
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `CLASS_INFORMATION`
--

LOCK TABLES `CLASS_INFORMATION` WRITE;
/*!40000 ALTER TABLE `CLASS_INFORMATION` DISABLE KEYS */;
INSERT INTO `CLASS_INFORMATION` VALUES ('asu_3u16472755e704e5fasul1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15849',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15852',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15854',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15856',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15859',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15861',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15863',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15865',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15868',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15870',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_15874',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_21662',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_21664',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_21666',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('asu_3u16472755e704e5fasul1_21668',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('MIT_1F20082ed799f4d14MITl1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('MIT_397367142947c4ec7MITl1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('MIT_8T11773e92d434df8MITl1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('MIT_96238198fb0774e40MITl1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('MIT_9u2563173b83c4c9dMITl1_',NULL,NULL,NULL,'unknown section',NULL,NULL,NULL),('study',NULL,NULL,NULL,NULL,NULL,NULL,NULL);
/*!40000 ALTER TABLE `CLASS_INFORMATION` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `PROBLEM_ATTEMPT`
--

DROP TABLE IF EXISTS `PROBLEM_ATTEMPT`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `PROBLEM_ATTEMPT` (
  `userName` varchar(20) NOT NULL,
  `sessionID` varchar(45) NOT NULL,
  `startTime` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `clientID` varchar(50) NOT NULL default 'fill in clientID',
  `classinformationID` int(10) unsigned NOT NULL,
  `userProblem` varchar(50) default NULL COMMENT 'The problem the user asks for when communicating with server. May or may not exist.',
  `userSection` varchar(50) default NULL COMMENT 'The section the user is enrolled in. may or may not exist. ',
  `extra` varchar(50) default NULL,
  PRIMARY KEY  (`clientID`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='Identifiers for multi-user sessions or copied sessions or ot';
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `PROBLEM_ATTEMPT`
--

LOCK TABLES `PROBLEM_ATTEMPT` WRITE;
/*!40000 ALTER TABLE `PROBLEM_ATTEMPT` DISABLE KEYS */;
INSERT INTO `PROBLEM_ATTEMPT` VALUES ('','','2011-12-26 22:26:47','bvds-2kt1a1324936320708',0,NULL,NULL,NULL),('','','2011-12-14 22:28:56','bvds-3fbd2a1323900338942',0,NULL,NULL,NULL);
/*!40000 ALTER TABLE `PROBLEM_ATTEMPT` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `STEP_TRANSACTION`
--

DROP TABLE IF EXISTS `STEP_TRANSACTION`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `STEP_TRANSACTION` (
  `tID` int(10) unsigned NOT NULL auto_increment,
  `clientID` varchar(50) NOT NULL,
  `client` text,
  `server` text,
  PRIMARY KEY  USING BTREE (`tID`,`clientID`),
  KEY `FK_step_problem` (`clientID`),
  CONSTRAINT `FK_step_problem` FOREIGN KEY (`clientID`) REFERENCES `PROBLEM_ATTEMPT` (`clientID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=3934609 DEFAULT CHARSET=latin1 COMMENT='each attempt has a series of transactions associated with it';
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `STEP_TRANSACTION`
--

LOCK TABLES `STEP_TRANSACTION` WRITE;
/*!40000 ALTER TABLE `STEP_TRANSACTION` DISABLE KEYS */;
INSERT INTO `STEP_TRANSACTION` VALUES (3697810,'bvds-3fbd2a1323900338942','{\"id\":24,\"method\":\"record-action\",\"params\":{\"time\":1397.464,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":24}'),(3697811,'bvds-3fbd2a1323900338942','{\"id\":25,\"method\":\"record-action\",\"params\":{\"time\":1400.046,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":25}'),(3730602,'bvds-3fbd2a1323900338942','{\"id\":26,\"method\":\"record-action\",\"params\":{\"time\":4185.456,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":26}'),(3730603,'bvds-3fbd2a1323900338942','{\"id\":27,\"method\":\"record-action\",\"params\":{\"time\":4186.997,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":27}'),(3730604,'bvds-3fbd2a1323900338942','{\"id\":28,\"method\":\"record-action\",\"params\":{\"time\":4548.082,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":28}'),(3730605,'bvds-3fbd2a1323900338942','{\"id\":29,\"method\":\"record-action\",\"params\":{\"time\":4549.703,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":29}'),(3783070,'bvds-3fbd2a1323900338942','{\"id\":30,\"method\":\"record-action\",\"params\":{\"time\":89660.166,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":30}'),(3783071,'bvds-3fbd2a1323900338942','{\"id\":31,\"method\":\"record-action\",\"params\":{\"time\":89662.558,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":31}'),(3816173,'bvds-3fbd2a1323900338942','{\"id\":32,\"method\":\"record-action\",\"params\":{\"time\":176848.749,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":32}'),(3816175,'bvds-3fbd2a1323900338942','{\"id\":33,\"method\":\"record-action\",\"params\":{\"time\":176850.937,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":33}'),(3867541,'bvds-2kt1a1324936320708','{\"id\":12,\"method\":\"record-action\",\"params\":{\"time\":2085.989,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":12}'),(3867542,'bvds-2kt1a1324936320708','{\"id\":13,\"method\":\"record-action\",\"params\":{\"time\":2090.787,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":13}'),(3882139,'bvds-2kt1a1324936320708','{\"id\":14,\"method\":\"record-action\",\"params\":{\"time\":4387.087,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":14}'),(3882140,'bvds-2kt1a1324936320708','{\"id\":15,\"method\":\"record-action\",\"params\":{\"time\":4388.139,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":15}'),(3882141,'bvds-2kt1a1324936320708','{\"id\":16,\"method\":\"record-action\",\"params\":{\"time\":4681.216,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":16}'),(3882142,'bvds-2kt1a1324936320708','{\"id\":17,\"method\":\"record-action\",\"params\":{\"time\":4682.159,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":17}'),(3882143,'bvds-2kt1a1324936320708','{\"id\":18,\"method\":\"record-action\",\"params\":{\"time\":4943.799,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"focus\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":18}'),(3882144,'bvds-2kt1a1324936320708','{\"id\":19,\"method\":\"record-action\",\"params\":{\"time\":4945.013,\"type\":\"window\",\"name\":\"canvas\",\"value\":\"blur\"},\"jsonrpc\":\"2.0\"}','{\"jsonrpc\":\"2.0\",\"id\":19}');
/*!40000 ALTER TABLE `STEP_TRANSACTION` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `STUDENT_STATE`
--

DROP TABLE IF EXISTS `STUDENT_STATE`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `STUDENT_STATE` (
  `userSection` varchar(50) NOT NULL COMMENT 'Should match section in CLASS_INFORMATION.',
  `userName` varchar(20) NOT NULL default '' COMMENT 'Empty string means that this is a section-wide model.  Otherwise, should match userName in PROBLEM_ATTEMPT',
  `model` varchar(20) NOT NULL default 'default' COMMENT '"default" is default student model; "client"/"server" are client/help system customizations.',
  `property` varchar(50) NOT NULL COMMENT 'For a student model, this is the name of the knowledge component.',
  `tID` int(10) unsigned NOT NULL,
  `value` text COMMENT 'Description of current state as json object or lisp sexpr.  NULL is equivalent to removing a property from the table.',
  UNIQUE KEY `userSection` (`userSection`,`userName`,`model`,`property`,`tID`),
  KEY `FK_model_class` (`userSection`),
  KEY `FK_step_state` (`tID`),
  CONSTRAINT `FK_model_class` FOREIGN KEY (`userSection`) REFERENCES `CLASS_INFORMATION` (`classSection`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `FK_step_state` FOREIGN KEY (`tID`) REFERENCES `STEP_TRANSACTION` (`tID`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='Changes to state.  Changes within a session may be consolida';
SET character_set_client = @saved_cs_client;

--
-- Dumping data for table `STUDENT_STATE`
--

LOCK TABLES `STUDENT_STATE` WRITE;
/*!40000 ALTER TABLE `STUDENT_STATE` DISABLE KEYS */;
/*!40000 ALTER TABLE `STUDENT_STATE` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2011-12-28 19:23:01
