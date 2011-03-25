ALTER TABLE `STUDENT_STATE`
  MODIFY COLUMN `userName` varchar(20) NOT NULL DEFAULT '' COMMENT 'Empty string means that this is a section-wide model.  Otherwise, should match userName in PROBLEM_ATTEMPT';
