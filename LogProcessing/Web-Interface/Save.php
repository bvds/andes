<?php
/*
  Copies the sessions for a problem solution (or portion thereof) to 
  a new session and returns a URL that will access the copy.
 */
require 'db-login.php';

if(strcmp($dbuser,'open')==0){
     $problem_attempt='OPEN_PROBLEM_ATTEMPT';
   } else {
     $problem_attempt='PROBLEM_ATTEMPT';
   }

$adminName = $_GET["au"];
$adminSection = $_GET["as"];
$userName = $_GET["u"];
$userProblem = $_GET["p"];
$userSection = $_GET["s"];
$tID = $_GET["t"];

date_default_timezone_set('UTC');
$clientID = $adminName . '_' . date("mdy").time();

$sql = $db->prepare("SELECT * FROM $problem_attempt AS P1 JOIN STEP_TRANSACTION AS P2 ON P1.clientID = P2.clientID WHERE P1.userName = '$userName' AND P1.userProblem = '$userProblem' AND P1.userSection = '$userSection' AND P2.tID <= $tID ORDER BY P2.tID");

$sql->execute();

if ($myrow = $sql->fetch()) {
  $maxQuery = $db->prepare("SELECT COALESCE(MAX(extra),0) AS max FROM PROBLEM_ATTEMPT WHERE userSection=? AND userName=?");
  $maxQuery->execute(array($adminSection, $adminName));
  $maxResultArray = $maxQuery->fetch();
  $maxValue = $maxResultArray["max"];
  $IncrVal = $maxValue+1;
  $query = $db->prepare("INSERT INTO PROBLEM_ATTEMPT(userName,clientID,userProblem,userSection,extra) VALUES(?,?,?,?,?)");
  $query->execute(array($adminName, $clientID, $userProblem, $adminSection, $IncrVal));

  $reviewQuery = $db->prepare("INSERT INTO SOLUTION_COPY(clientID,tID) VALUES(?,?)");
  $reviewQuery->execute(array($clientID, $tID));

  do {
    $insertQuery = $db->prepare("INSERT INTO STEP_TRANSACTION(clientID,client,server) VALUES(?,?,?)");
    $insertQuery->execute(array($clientID, $myrow["client"], $myrow["server"]));
  } while ($myrow = $sql->fetch());
}
echo "/web-UI/index.html?s=$adminSection&u=$adminName&p=$userProblem&e=$IncrVal";
?>
