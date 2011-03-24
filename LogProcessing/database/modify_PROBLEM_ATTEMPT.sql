ALTER TABLE `PROBLEM_ATTEMPT` DROP COLUMN classinformationID,
      DROP COLUMN sessionID,
      ALTER COLUMN userSection SET DEFAULT 'defaultSection',
      ADD KEY  `FK_problemstate_classinformation` (`userSection`);
ALTER TABLE `PROBLEM_ATTEMPT` ADD CONSTRAINT `FK_problemstate_classinformation` FOREIGN KEY (`userSection`) REFERENCES `CLASS_INFORMATION` (`classSection`) ON DELETE CASCADE ON UPDATE CASCADE;
