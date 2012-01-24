/*
    Set up classes for U Wisconson Platteville course
    used in random  help experiment.

    This file should have a more appropriate home, Bug #1940.
*/

-- for rerunning old sessions through help, for testing
DELETE FROM PROBLEM_ATTEMPT WHERE clientID LIKE '\_%';
INSERT into CLASS_INFORMATION (classSection,description) values ('random-help-test','test section');
INSERT into CLASS_INFORMATION (classSection,description) values ('MIT_96238198fb0774e40MITl1_','mit fall 2011');
INSERT into CLASS_INFORMATION (classSection,description) values ('MIT_397367142947c4ec7MITl1_','mit fall 2011');
INSERT into CLASS_INFORMATION (classSection,description) values ('asu_3u16472755e704e5fasul1_15865','OSU fall 2011');
INSERT into CLASS_INFORMATION (classSection,description) values ('asu_3u16472755e704e5fasul1_15854','OSU fall 2011');

-- For setting up UW Platteville sections for experiment
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_51421910795174fcfuwplattl1_','Pawl, physics 2240A1');
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_6l13051599e174fb5uwplattl1_','Pawl, physics 2240A2');
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_2Y1305989a5174f1cuwplattl1_','Pawl, physics 2340C1');
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_3n13056a8a6174fbeuwplattl1_','Pawl, physics 2340C2');
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_8p130495419184f26uwplattl1_','Scaife, Physics 2240');
INSERT into CLASS_INFORMATION (classSection,description) values ('uwplatt_9047621c019184fdbuwplattl1_','Scaife, Physics 2340');
