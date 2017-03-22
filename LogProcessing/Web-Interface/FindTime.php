<?php

require "db-login.php";

$sectionName = $_POST['userSection'];
$hintType=$_POST['HintType'];
$user=$_POST['userName'];
$prob=$_POST['userProblem'];

if($user=='') {
  $userName = "";
} else {
  $userName="P.userName REGEXP '$user' AND";
} 
if($sectionName=='') {
  $userSection='';
}else {
  $userSection = "P.userSection REGEXP  '$sectionName' AND";
}
if($prob) {
  $userProblem="P.userProblem='$prob' AND";
}

$sql = $db->prepare("select Count(DISTINCT userName) from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem $userSection P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.client like '%\"time\"%' order by startTime");
$sql->execute();
if ($myrow = $sql->fetch()) {
  $TotUsers=$myrow["Count(DISTINCT userName)"];
  echo "<p align='center'><font color=\"red\">Number of Students who attempted $prob are $TotUsers</p>";
}

$sql = $db->prepare("select Count(DISTINCT userName) from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem P.userSection='$userSection' and P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.server like '%\"correct_answer_entries_v_answer_entries\"%' and S.client like '%\"time\"%' and S.server like '%\"1\/%' order by startTime");
$sql->execute();
if ($myrow = $sql->fetch()) {
  $TotUsers=$myrow["Count(DISTINCT userName)"];
  //  echo "<p align='center'><font color=\"red\">Number of Students who completed $prob are $TotUsers</p>";
}

$sql = $db->prepare("select * from OPEN_PROBLEM_ATTEMPT as P,STEP_TRANSACTION as S where $userName $userProblem P.userSection='$userSection' and P.clientID=S.clientID and S.server like '%\"action\":\"problem-closed\"%' and S.server like '%\"correct_answer_entries_v_answer_entries\"%' and S.client like '%\"time\"%' order by startTime");

$sql->execute();
$Count=0;
if ($myrow = $sql->fetch()) {
  echo "<table border=1>";
  echo "<tr><th>UserName</th><th>Client</th><th>Server</th></tr>";  
  do {
    $userN=$myrow["userName"];
    $client=$myrow["client"];
    $server=$myrow["server"];
    $done=strstr($server,"1\/");
    if($done) {
      $Count=$Count+1;
      echo "<tr><td>$userN</td><td>$client</td><td>$server</td></tr>";
    }
    else
      echo "<tr><td>$userN</td><td>$client</td><td bgcolor=\"red\">$server</td></tr>";
  } while ($myrow = $sql->fetch());
  echo "</table>"; 
  echo "<br><p align='center'> Number of Students who completed this problem are : $Count</p>";
}

?>
