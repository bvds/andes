ALTER TABLE `PROBLEM_ATTEMPT`
  MODIFY COLUMN `extra` varchar(50) DEFAULT NULL, COMMENT 'Identifiers for multi-user sessions or copied sessions or other such stuff.';
