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

/*
  Rerunning sessions through server

sbcl --dynamic-space-size 1000

(rhelp)
(setf webserver:*debug* nil) 
(setf *simulate-loaded-server* nil)
(start-help :db "andes_test" :port 8081)
(random-help-experiment::set-experiment-probability 0.5)

use andes_test;
DELETE FROM PROBLEM_ATTEMPT WHERE clientID LIKE '\_%';
 
  May need to run above inserts into CLASS_INFORMATION


Steps for getting mysql log file:
 Stop Andes
  Restart mysqld to get new binary log file
 start Andes
 
ls -l -t /var/lib/mysql  # to get name of latest log file

mysqlbinlog --start-datetime="2012-01-25 14:00:00" --stop-datetime="2012-01-25 16:00:00" mysqlLOGb.000028 > /tmp/junk.out
  
*/
